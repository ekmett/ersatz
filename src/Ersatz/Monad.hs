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
import Control.Monad.State
import Data.IntSet as IntSet
import Data.HashMap.Strict as HashMap
import Data.Monoid
import Ersatz.Internal.Literal
import Ersatz.Internal.Problem
import Ersatz.Internal.StableName
import System.IO.Unsafe

newtype SAT a = SAT { runSAT :: Problem -> (a, Problem) }

instance Functor SAT where
  fmap f (SAT m) = SAT $ \s -> case m s of
    (a, t) -> (f a, t)
  {-# INLINE fmap #-}

instance Applicative SAT where
  pure a = SAT $ \s -> (a, s)
  {-# INLINE pure #-}

  SAT m <*> SAT n = SAT $ \s -> case m s of
    (f, t) -> case n t of
      (a, u) -> (f a, u)
  {-# INLINE (<*>) #-}

instance Monad SAT where
  return a = SAT $ \s -> (a, s)
  {-# INLINE return #-}

  SAT m >>= f = SAT $ \s -> case m s of
    (a, t) -> runSAT (f a) t
  {-# INLINE (>>=) #-}

instance MonadSAT SAT where
  literalExists = SAT $ \qbf -> let !qbfLastAtom' = qbfLastAtom qbf + 1 in
    (Literal qbfLastAtom', qbf { qbfLastAtom = qbfLastAtom' })

  literalForall = SAT $ \qbf -> let !qbfLastAtom' = qbfLastAtom qbf + 1 in
    ( Literal qbfLastAtom', qbf { qbfLastAtom = qbfLastAtom', qbfUniversals = IntSet.insert qbfLastAtom' (qbfUniversals qbf) })

  assertFormula formula = SAT $ \qbf -> ((), qbf { qbfFormula = qbfFormula qbf <> formula })

  generateLiteral a f = SAT $ \qbf -> case HashMap.lookup sn (qbfSNMap qbf) of
      Just l  -> (l, qbf)
      Nothing | !qbfLastAtom' <- qbfLastAtom qbf + 1, !l <- Literal qbfLastAtom' ->
        case runSAT (f l) qbf { qbfSNMap = HashMap.insert sn l (qbfSNMap qbf), qbfLastAtom = qbfLastAtom' } of
           ((), qbf') -> (l, qbf')
    where sn = unsafePerformIO (makeStableName' a)

class (Monad m, Applicative m) => MonadSAT m where
  literalExists   :: m Literal
  default literalExists :: (MonadSAT n, MonadTrans t, m ~ t n) => m Literal
  literalExists = lift literalExists

  literalForall   :: m Literal
  default literalForall :: (MonadSAT n, MonadTrans t, m ~ t n) => m Literal
  literalForall = lift literalForall

  assertFormula   :: Formula -> m ()
  default assertFormula :: (MonadSAT n, MonadTrans t, m ~ t n) => Formula -> m ()
  assertFormula = lift . assertFormula

  generateLiteral :: a -> (Literal -> SAT ()) -> m Literal
  default generateLiteral :: (MonadSAT n, MonadTrans t, m ~ t n) => a -> (Literal -> SAT ()) -> m Literal
  generateLiteral a f = lift $ generateLiteral a f

