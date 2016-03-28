{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_HADDOCK not-home #-}
--------------------------------------------------------------------
-- |
-- Copyright :  © Edward Kmett 2010-2014, Johan Kiviniemi 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ersatz.Equatable
  ( Equatable(..)
  , GEquatable(..)
  ) where

import Prelude hiding ((&&),(||),not,and,or,all,any)

import Ersatz.Bit
import GHC.Generics
import Data.IntMap (IntMap)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap

infix  4 ===, /==

-- | Instances for this class for arbitrary types can be automatically derived from 'Generic'.
class Equatable t where
  -- | Compare for equality within the SAT problem.
  (===) :: t -> t -> Bit
#ifndef HLINT
  default (===) :: (Generic t, GEquatable (Rep t)) => t -> t -> Bit
  a === b = from a ===# from b
#endif

  -- | Compare for inequality within the SAT problem.
  (/==) :: t -> t -> Bit

  a /== b = not (a === b)

instance Equatable Bit where
  a === b = not (xor a b)
  (/==) = xor

instance (Eq k, Equatable v) => Equatable (Map k v) where
  x === y
    | Map.keys x == Map.keys y = Map.elems x === Map.elems y
    | otherwise                = false

instance Equatable v => Equatable (IntMap v) where
  x === y
    | IntMap.keys x == IntMap.keys y = IntMap.elems x === IntMap.elems y
    | otherwise                      = false

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

instance GEquatable V1 where
  x ===# y = x `seq` y `seq` error "GEquatable[V1].===#"

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
