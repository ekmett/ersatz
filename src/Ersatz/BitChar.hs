{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
--------------------------------------------------------------------
-- |
-- Copyright :  © Eric Mertens 2010-2014
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ersatz.BitChar where

import Data.Char (chr,ord)
import Control.Monad (replicateM)
import Prelude hiding ((&&))
import Data.Typeable (Typeable)

import Ersatz.Bit
import Ersatz.Bits
import Ersatz.Codec
import Ersatz.Equatable
import Ersatz.Orderable
import Ersatz.Variable

-- | List of 'BitChar' intended to be used as the representation for 'String'.
type BitString = [BitChar]

-- | Encoding of the full range of 'Char' values.
newtype BitChar = BitChar Bits
  deriving (Show,Typeable)

instance Codec BitChar where
  type Decoded BitChar = Char
  encode                = BitChar . fromIntegral . ord
  decode s (BitChar xs) = fmap (chr . fromIntegral) (decode s xs)

instance Equatable BitChar where
  BitChar xs === BitChar ys = xs === ys
  BitChar xs /== BitChar ys = xs /== ys

instance Orderable BitChar where
  BitChar xs <?  BitChar ys = xs <?  ys
  BitChar xs <=? BitChar ys = xs <=? ys

instance Variable BitChar where
  literally m =
       -- Char upperbound is 0x10ffff, so only set
       -- the high bit when the next 4 bits are 0

    do x  <- literally m
       xs <- replicateM 20 (literally m)

       let x' = x && nor (take 4 xs)
           n  = Bits (reverse (x':xs)) -- Bits is little endian

       return (BitChar n)
