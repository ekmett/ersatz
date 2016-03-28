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
module Ersatz.Orderable
  ( Orderable(..)
  , GOrderable(..)
  ) where

import Prelude hiding ((&&),(||),not,and,or,all,any)

import Ersatz.Bit
import Ersatz.Equatable
import GHC.Generics

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
