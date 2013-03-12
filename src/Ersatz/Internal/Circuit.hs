{-# LANGUAGE DeriveDataTypeable #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2010-2013, Johan Kiviniemi 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ersatz.Internal.Circuit
  ( Circuit(..)
  ) where

import Control.Applicative
import Data.Foldable (Foldable, foldMap)
import Data.Monoid
import Data.Sequence (Seq)
import Data.Traversable (Traversable, traverse)
import Data.Typeable
import Ersatz.Internal.Literal

-- | This is used to observe the directed graph with sharing of how multiple
-- 'Ersatz.Bit.Bit' values are related.
data Circuit c
  = And (Seq c)
  | Or (Seq c)
  | Xor c c
  | Mux c c c  -- ^ False branch, true branch, predicate/selector branch
  | Not c
  | Var !Literal
  deriving (Show, Typeable)

instance Functor Circuit where
  fmap f (And as) = And (fmap f as)
  fmap f (Or as) = Or (fmap f as)
  fmap f (Xor a b) = Xor (f a) (f b)
  fmap f (Mux a b c) = Mux (f a) (f b) (f c)
  fmap f (Not a) = Not (f a)
  fmap _ (Var l) = Var l

instance Foldable Circuit where
  foldMap f (And as) = foldMap f as
  foldMap f (Or as) = foldMap f as
  foldMap f (Xor a b) = f a <> f b
  foldMap f (Mux a b c) = f a <> f b <> f c
  foldMap f (Not a) = f a
  foldMap _ Var{} = mempty

instance Traversable Circuit where
  traverse f (And as) = And <$> traverse f as
  traverse f (Or as) = Or <$> traverse f as
  traverse f (Xor a b) = Xor <$> f a <*> f b
  traverse f (Mux a b c) = Mux <$> f a <*> f b <*> f c
  traverse f (Not a) = Not <$> f a
  traverse _ (Var l) = pure (Var l)
