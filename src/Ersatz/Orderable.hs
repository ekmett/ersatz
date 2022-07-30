{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
#ifndef HLINT
{-# LANGUAGE DefaultSignatures #-}
#endif
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
module Ersatz.Orderable
  ( Orderable(..)
  , GOrderable(..)
  ) where

import Prelude hiding ((&&),(||),not,and,or,all,any)

import Data.Foldable (toList)
import Data.Int
import Data.Void
import Data.Word
import Ersatz.Bit
import Ersatz.Equatable
import GHC.Generics
import Numeric.Natural
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Tree as Tree

infix  4 <?, <=?, >=?, >?

-- | Instances for this class for arbitrary types can be automatically derived from 'Generic'.
class Equatable t => Orderable t where
  -- | Compare for less-than within the SAT problem.
  (<?)  :: t -> t -> Bit

  -- | Compare for less-than or equal-to within the SAT problem.
  (<=?) :: t -> t -> Bit
  x <=? y = x === y || x <? y
#ifndef HLINT
  default (<?) :: (Generic t, GOrderable (Rep t)) => t -> t -> Bit
  a <? b = from a <?# from b
#endif

  -- | Compare for greater-than or equal-to within the SAT problem.
  (>=?) :: t -> t -> Bit
  x >=? y = y <=? x

  -- | Compare for greater-than within the SAT problem.
  (>?) :: t -> t -> Bit
  x >? y = y <? x


instance Orderable Bit where
  a <?  b = not a && b
  a <=? b = not a || b

-- | Compare by lexicographic order on sorted key-value pairs
instance (Ord k, Orderable v) => Orderable (Map.Map k v) where
  x <?  y = assocsLt (Map.assocs x) (Map.assocs y)
  x <=? y = assocsLe (Map.assocs x) (Map.assocs y)

-- | Compare by lexicographic order on sorted key-value pairs
instance Orderable v => Orderable (IntMap.IntMap v) where
  x <?  y = assocsLt (IntMap.assocs x) (IntMap.assocs y)
  x <=? y = assocsLe (IntMap.assocs x) (IntMap.assocs y)

assocsLt :: (Ord k, Orderable v) => [(k,v)] -> [(k,v)] -> Bit
assocsLt _ [] = false
assocsLt [] _ = true
assocsLt ((k1,v1):xs) ((k2,v2):ys) =
  case compare k1 k2 of
    LT -> true
    GT -> false
    EQ -> v1 <? v2 || v1 === v2 && assocsLt xs ys

assocsLe :: (Ord k, Orderable v) => [(k,v)] -> [(k,v)] -> Bit
assocsLe [] _ = true
assocsLe _ [] = false
assocsLe ((k1,v1):xs) ((k2,v2):ys) =
  case compare k1 k2 of
    LT -> true
    GT -> false
    EQ -> v1 <? v2 || v1 === v2 && assocsLe xs ys

-- | Compare by lexicographic order on elements
instance Orderable v => Orderable (Seq.Seq v) where
  x <?  y = toList x <?  toList y
  x <=? y = toList x <=? toList y

-- | Compare by lexicographic order on: root node, list of children
instance Orderable a => Orderable (Tree.Tree a) where
  Tree.Node x xs <?  Tree.Node y ys = (x,xs) <?  (y,ys)
  Tree.Node x xs <=? Tree.Node y ys = (x,xs) <=? (y,ys)

instance (Orderable a, Orderable b) => Orderable (a,b)
instance (Orderable a, Orderable b, Orderable c) => Orderable (a,b,c)
instance (Orderable a, Orderable b, Orderable c, Orderable d) => Orderable (a,b,c,d)
instance (Orderable a, Orderable b, Orderable c, Orderable d, Orderable e) => Orderable (a,b,c,d,e)
instance (Orderable a, Orderable b, Orderable c, Orderable d, Orderable e, Orderable f) => Orderable (a,b,c,d,e,f)
instance (Orderable a, Orderable b, Orderable c, Orderable d, Orderable e, Orderable f, Orderable g) => Orderable (a,b,c,d,e,f,g)
instance Orderable a => Orderable (Maybe a)
instance (Orderable a, Orderable b) => Orderable (Either a b)

-- | Lexicographic order
instance Orderable a => Orderable [a] where
#ifndef HLINT
  []   <? []   = false
  x:xs <? y:ys = x === y && xs <? ys
              || x <?  y
  []   <? _    = true
  _    <? []   = false

  []   <=? _    = true
  x:xs <=? y:ys = x === y && xs <=? ys
               || x <?  y
  _    <=? []   = false
#endif

class GEquatable f => GOrderable f where
  (<?#) :: f a -> f a -> Bit
  (<=?#) :: f a -> f a -> Bit

instance GOrderable U1 where
  U1 <?#  U1 = false
  U1 <=?# U1 = true

instance GOrderable V1 where
  x <?# y = x `seq` y `seq` error "GOrderable[V1].<?#"
  x <=?# y = x `seq` y `seq` error "GOrderable[V1].<=?#"

instance (GOrderable f, GOrderable g) => GOrderable (f :*: g) where
  (a :*: b) <?#  (c :*: d) = (a <?# c) || (a ===# c && b <?# d)
  (a :*: b) <=?# (c :*: d) = (a <?# c) || (a ===# c && b <=?# d)

instance (GOrderable f, GOrderable g) => GOrderable (f :+: g) where
  L1 _ <?# R1 _ = true
  L1 a <?# L1 b = a <?# b
  R1 a <?# R1 b = a <?# b
  R1 _ <?# L1 _ = false

  L1 _ <=?# R1 _ = true
  L1 a <=?# L1 b = a <=?# b
  R1 a <=?# R1 b = a <=?# b
  R1 _ <=?# L1 _ = false

instance GOrderable f => GOrderable (M1 i c f) where
  M1 x <?#  M1 y = x <?#  y
  M1 x <=?# M1 y = x <=?# y

instance Orderable a => GOrderable (K1 i a) where
  K1 a <?#  K1 b = a <?  b
  K1 a <=?# K1 b = a <=? b

-- Boring instances that end up being useful when deriving Orderable with Generics

instance Orderable ()       where _ <?  _ = false
                                  _ <=? _ = true
instance Orderable Void     where x <?  y = x `seq` y `seq` error "Orderable[Void].<?"
                                  x <=? y = x `seq` y `seq` error "Orderable[Void].<=?"
instance Orderable Int      where x <?  y = bool (x <  y)
                                  x <=? y = bool (x <= y)
instance Orderable Integer  where x <?  y = bool (x <  y)
                                  x <=? y = bool (x <= y)
instance Orderable Natural  where x <?  y = bool (x <  y)
                                  x <=? y = bool (x <= y)
instance Orderable Word     where x <?  y = bool (x <  y)
                                  x <=? y = bool (x <= y)
instance Orderable Word8    where x <?  y = bool (x <  y)
                                  x <=? y = bool (x <= y)
instance Orderable Word16   where x <?  y = bool (x <  y)
                                  x <=? y = bool (x <= y)
instance Orderable Word32   where x <?  y = bool (x <  y)
                                  x <=? y = bool (x <= y)
instance Orderable Word64   where x <?  y = bool (x <  y)
                                  x <=? y = bool (x <= y)
instance Orderable Int8     where x <?  y = bool (x <  y)
                                  x <=? y = bool (x <= y)
instance Orderable Int16    where x <?  y = bool (x <  y)
                                  x <=? y = bool (x <= y)
instance Orderable Int32    where x <?  y = bool (x <  y)
                                  x <=? y = bool (x <= y)
instance Orderable Int64    where x <?  y = bool (x <  y)
                                  x <=? y = bool (x <= y)
instance Orderable Char     where x <?  y = bool (x <  y)
                                  x <=? y = bool (x <= y)
instance Orderable Float    where x <?  y = bool (x <  y)
                                  x <=? y = bool (x <= y)
instance Orderable Double   where x <?  y = bool (x <  y)
                                  x <=? y = bool (x <= y)
instance Orderable Ordering where x <?  y = bool (x <  y)
                                  x <=? y = bool (x <= y)
instance Orderable Bool     where x <?  y = bool (x <  y)
                                  x <=? y = bool (x <= y)
