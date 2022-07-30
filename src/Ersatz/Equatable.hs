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
import GHC.Natural
import Data.IntMap (IntMap)
import Data.Foldable (toList)
import Data.Map (Map)
import Data.Int
import Data.Word
import Data.Void
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.Sequence as Seq
import qualified Data.Tree as Tree

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

instance (Eq k, Equatable v) => Equatable (Map k v) where
  x === y
    | Map.keys x == Map.keys y = Map.elems x === Map.elems y
    | otherwise                = false

instance Equatable v => Equatable (IntMap v) where
  x === y
    | IntMap.keys x == IntMap.keys y = IntMap.elems x === IntMap.elems y
    | otherwise                      = false

instance Equatable v => Equatable (Seq.Seq v) where
  x === y
    | Seq.length x == Seq.length y = toList x === toList y
    | otherwise                    = false

instance (Equatable a, Equatable b) => Equatable (a,b)
instance (Equatable a, Equatable b, Equatable c) => Equatable (a,b,c)
instance (Equatable a, Equatable b, Equatable c, Equatable d) => Equatable (a,b,c,d)
instance (Equatable a, Equatable b, Equatable c, Equatable d, Equatable e) => Equatable (a,b,c,d,e)
instance (Equatable a, Equatable b, Equatable c, Equatable d, Equatable e, Equatable f) => Equatable (a,b,c,d,e,f)
instance (Equatable a, Equatable b, Equatable c, Equatable d, Equatable e, Equatable f, Equatable g) => Equatable (a,b,c,d,e,f,g)
instance Equatable a => Equatable (Maybe a)
instance Equatable a => Equatable [a]
instance Equatable a => Equatable (NonEmpty a)
instance (Equatable a, Equatable b) => Equatable (Either a b)
instance Equatable a => Equatable (Tree.Tree a)

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

-- Boring instances that end up being useful when deriving Equatable with Generics

instance Equatable ()       where _ === _ = true
instance Equatable Void     where x === y = x `seq` y `seq` error "Equatable[Void].==="
instance Equatable Int      where x === y = bool (x == y)
instance Equatable Integer  where x === y = bool (x == y)
instance Equatable Natural  where x === y = bool (x == y)
instance Equatable Word     where x === y = bool (x == y)
instance Equatable Word8    where x === y = bool (x == y)
instance Equatable Word16   where x === y = bool (x == y)
instance Equatable Word32   where x === y = bool (x == y)
instance Equatable Word64   where x === y = bool (x == y)
instance Equatable Int8     where x === y = bool (x == y)
instance Equatable Int16    where x === y = bool (x == y)
instance Equatable Int32    where x === y = bool (x == y)
instance Equatable Int64    where x === y = bool (x == y)
instance Equatable Char     where x === y = bool (x == y)
instance Equatable Float    where x === y = bool (x == y)
instance Equatable Double   where x === y = bool (x == y)
instance Equatable Ordering where x === y = bool (x == y)
instance Equatable Bool     where x === y = bool (x == y)
