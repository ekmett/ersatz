{-# LANGUAGE TypeFamilies, TypeOperators, FlexibleInstances, DeriveDataTypeable, DeriveGeneric, DefaultSignatures, FlexibleContexts, UndecidableInstances #-}
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
module Ersatz.Equatable
  ( Equatable(..)
  ) where

import Prelude hiding ((&&),(||),not,and,or)
import qualified Prelude

import Ersatz.Bit
import GHC.Generics

infix  4 ===, /==

-- | Instances for this class for arbitrary types can be automatically derived from 'Generic'.
class Equatable t where
  -- | Compare for equality within the SAT problem.
  (===) :: t -> t -> Bit
  default (===) :: (Generic t, GEquatable (Rep t)) => t -> t -> Bit
  a === b = from a ===# from b

  -- | Compare for inequality within the SAT problem.
  (/==) :: t -> t -> Bit

  a /== b = not (a === b)

instance Equatable Bit where
  a === b = not (xor a b)
  (/==) = xor

instance (Equatable a, Equatable b) => Equatable (a,b)
instance (Equatable a, Equatable b, Equatable c) => Equatable (a,b,c)
instance (Equatable a, Equatable b, Equatable c, Equatable d) => Equatable (a,b,c,d)
instance (Equatable a, Equatable b, Equatable c, Equatable d, Equatable e) => Equatable (a,b,c,d,e)
instance (Equatable a, Equatable b, Equatable c, Equatable d, Equatable e, Equatable f) => Equatable (a,b,c,d,e,f)
instance (Equatable a, Equatable b, Equatable c, Equatable d, Equatable e, Equatable f, Equatable g) => Equatable (a,b,c,d,e,f,g)
instance Equatable a => Equatable (Maybe a)
instance Equatable a => Equatable [a]
instance (Equatable a, Equatable b) => Equatable (Either a b)

class GEquatable f where
  (===#) :: f a -> f a -> Bit

instance GEquatable U1 where
  U1 ===# U1 = true

instance (GEquatable f, GEquatable g) => GEquatable (f :*: g) where
  (a :*: b) ===# (c :*: d) = (a ===# c) && (b ===# d)

instance (GEquatable f, GEquatable g) => GEquatable (f :+: g) where
  L1 a ===# L1 b = a ===# b
  R1 a ===# R1 b = a ===# b
  _ ===# _ = false

instance GEquatable f => GEquatable (M1 i c f) where
  M1 x ===# M1 y = x ===# y

instance Equatable a => GEquatable (K1 i a) where
  K1 a ===# K1 b = a === b
