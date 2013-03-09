{-# LANGUAGE Rank2Types, GeneralizedNewtypeDeriving, TypeOperators, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, PatternGuards, DeriveDataTypeable, DefaultSignatures, TypeFamilies, BangPatterns #-}
{-# OPTIONS_HADDOCK not-home #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2010-2013, Johan Kiviniemi 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ersatz.Monad
  (
  -- * The SAT Monad
    SAT(..)
  , MonadSAT(..)
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.State.Lazy as Lazy
import Control.Monad.State.Strict as Strict
import Data.IntSet as IntSet
import Data.HashMap.Strict as HashMap
import Data.Monoid
import Ersatz.Internal.Formula
import Ersatz.Internal.Literal
import Ersatz.Internal.StableName
import Ersatz.Problem
import System.IO.Unsafe

newtype SAT m a = SAT { runSAT :: forall r. (a -> Problem -> m r) -> Problem -> m r }

instance Functor (SAT m) where
  fmap f (SAT m) = SAT $ \k -> m (k . f)
  {-# INLINE fmap #-}

instance Applicative (SAT m) where
  pure a = SAT $ \k -> k a
  {-# INLINE pure #-}

  (<*>) = ap
  {-# INLINE (<*>) #-}

instance Monad (SAT m) where
  return a = SAT $ \k -> k a
  {-# INLINE return #-}

  SAT m >>= f = SAT $ \k -> m (\a -> runSAT (f a) k)
  {-# INLINE (>>=) #-}

instance MonadTrans SAT where
  lift m = SAT $ \k p -> do
    a <- m
    k a p
  {-# INLINE lift #-}

instance MonadIO m => MonadIO (SAT m) where
  liftIO m = SAT $ \k p -> do
    a <- liftIO m
    k a p
  {-# INLINE liftIO #-}

class (Applicative m, Monad m) => MonadSAT m where
  sat :: (Problem -> (a, Problem)) -> m a
  default sat :: (MonadTrans t, MonadSAT n, m ~ t n) => (Problem -> (a, Problem)) -> m a
  sat = lift . sat

  literalExists :: m Literal
  literalExists = sat $ \qbf -> let !qbfLastAtom' = qbfLastAtom qbf + 1 in
    (Literal qbfLastAtom', qbf { qbfLastAtom = qbfLastAtom' })
  {-# INLINE literalExists #-}

  literalForall :: m Literal
  literalForall = sat $ \qbf -> let !qbfLastAtom' = qbfLastAtom qbf + 1 in
    ( Literal qbfLastAtom', qbf { qbfLastAtom = qbfLastAtom', qbfUniversals = IntSet.insert qbfLastAtom' (qbfUniversals qbf) })
  {-# INLINE literalForall #-}

  assertFormula :: Formula -> m ()
  assertFormula formula = sat $ \qbf -> ((), qbf { qbfFormula = qbfFormula qbf <> formula })
  {-# INLINE assertFormula #-}

  generateLiteral :: a -> (forall n. Literal -> SAT n ()) -> m Literal
  generateLiteral a f = sat $ \qbf -> case HashMap.lookup sn (qbfSNMap qbf) of
    Just l  -> (l, qbf)
    Nothing | !qbfLastAtom' <- qbfLastAtom qbf + 1, !l <- Literal qbfLastAtom' ->
      runSAT (f l) (\_ s -> (l, s)) qbf { qbfSNMap = HashMap.insert sn l (qbfSNMap qbf), qbfLastAtom = qbfLastAtom' }
    where sn = unsafePerformIO (makeStableName' a)
  {-# INLINE generateLiteral #-}

instance MonadSAT (SAT m) where
  sat f = SAT $ \k s -> case f s of
    (a, t) -> k a t

instance MonadSAT m => MonadSAT (Lazy.StateT s m)
instance MonadSAT m => MonadSAT (Strict.StateT s m)
