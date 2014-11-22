{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Safe #-}
--------------------------------------------------------------------
-- |
-- Copyright :  © Edward Kmett 2010-2014, Johan Kiviniemi 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ersatz.Bits
  ( Bit1(..), Bit2(..), Bit3(..), Bit4(..), Bit5(..), Bit6(..), Bit7(..), Bit8(..)
  , full_adder, half_adder
  ) where

import Prelude hiding ((&&), (||), and, or, not)

import Control.Applicative
import Data.Bits (Bits, (.&.), (.|.), shiftL, shiftR)
import Data.List (foldl')
import Data.Typeable
import Data.Word (Word8)
import GHC.Generics

import Ersatz.Bit
import Ersatz.Codec
import Ersatz.Equatable
import Ersatz.Variable

-- | A container of 1 'Bit' that 'encode's from and 'decode's to 'Word8'
newtype Bit1 = Bit1 Bit deriving (Show,Typeable,Generic)
-- | A container of 2 'Bit's that 'encode's from and 'decode's to 'Word8'
data Bit2 = Bit2 !Bit !Bit deriving (Show,Typeable,Generic)
-- | A container of 3 'Bit's that 'encode's from and 'decode's to 'Word8'
data Bit3 = Bit3 !Bit !Bit !Bit deriving (Show,Typeable,Generic)
-- | A container of 4 'Bit's that 'encode's from and 'decode's to 'Word8'
data Bit4 = Bit4 !Bit !Bit !Bit !Bit deriving (Show,Typeable,Generic)
-- | A container of 5 'Bit's that 'encode's from and 'decode's to 'Word8'
data Bit5 = Bit5 !Bit !Bit !Bit !Bit !Bit deriving (Show,Typeable,Generic)
-- | A container of 6 'Bit's that 'encode's from and 'decode's to 'Word8'
data Bit6 = Bit6 !Bit !Bit !Bit !Bit !Bit !Bit deriving (Show,Typeable,Generic)
-- | A container of 7 'Bit's that 'encode's from and 'decode's to 'Word8'
data Bit7 = Bit7 !Bit !Bit !Bit !Bit !Bit !Bit !Bit deriving (Show,Typeable,Generic)
-- | A container of 8 'Bit's that 'encode's from and 'decode's to 'Word8'
data Bit8 = Bit8 !Bit !Bit !Bit !Bit !Bit !Bit !Bit !Bit deriving (Show,Typeable,Generic)

instance Boolean Bit1
instance Boolean Bit2
instance Boolean Bit3
instance Boolean Bit4
instance Boolean Bit5
instance Boolean Bit6
instance Boolean Bit7
instance Boolean Bit8

instance Equatable Bit1
instance Equatable Bit2
instance Equatable Bit3
instance Equatable Bit4
instance Equatable Bit5
instance Equatable Bit6
instance Equatable Bit7
instance Equatable Bit8

instance Variable Bit1
instance Variable Bit2
instance Variable Bit3
instance Variable Bit4
instance Variable Bit5
instance Variable Bit6
instance Variable Bit7
instance Variable Bit8

instance Codec Bit1 where
  type Decoded Bit1 = Word8
  decode s (Bit1 a) = boolsToNum1 <$> decode s a
  encode i = Bit1 a where (a:_) = bitsOf i

instance Codec Bit2 where
  type Decoded Bit2 = Word8
  decode s (Bit2 a b) = boolsToNum2 <$> decode s a <*> decode s b
  encode i = Bit2 a b where (b:a:_) = bitsOf i

instance Codec Bit3 where
  type Decoded Bit3 = Word8
  decode s (Bit3 a b c) = boolsToNum3 <$> decode s a <*> decode s b <*> decode s c
  encode i = Bit3 a b c where (c:b:a:_) = bitsOf i

instance Codec Bit4 where
  type Decoded Bit4 = Word8
  decode s (Bit4 a b c d) = boolsToNum4 <$> decode s a <*> decode s b <*> decode s c <*> decode s d
  encode i = Bit4 a b c d where (d:c:b:a:_) = bitsOf i

instance Codec Bit5 where
  type Decoded Bit5 = Word8
  decode s (Bit5 a b c d e) = boolsToNum5 <$> decode s a <*> decode s b <*> decode s c <*> decode s d <*> decode s e
  encode i = Bit5 a b c d e where (e:d:c:b:a:_) = bitsOf i

instance Codec Bit6 where
  type Decoded Bit6 = Word8
  decode s (Bit6 a b c d e f) = boolsToNum6 <$> decode s a <*> decode s b <*> decode s c <*> decode s d <*> decode s e <*> decode s f
  encode i = Bit6 a b c d e f where (f:e:d:c:b:a:_) = bitsOf i

instance Codec Bit7 where
  type Decoded Bit7 = Word8
  decode s (Bit7 a b c d e f g) = boolsToNum7 <$> decode s a <*> decode s b <*> decode s c <*> decode s d <*> decode s e <*> decode s f <*> decode s g
  encode i = Bit7 a b c d e f g where (g:f:e:d:c:b:a:_) = bitsOf i

instance Codec Bit8 where
  type Decoded Bit8 = Word8
  decode s (Bit8 a b c d e f g h) = boolsToNum8 <$> decode s a <*> decode s b <*> decode s c <*> decode s d <*> decode s e <*> decode s f <*> decode s g <*> decode s h
  encode i = Bit8 a b c d e f g h where (h:g:f:e:d:c:b:a:_) = bitsOf i


boolsToNum1 :: Bool -> Word8
boolsToNum1 = boolToNum

boolsToNum2 :: Bool -> Bool -> Word8
boolsToNum2 a b = boolsToNum [a,b]

boolsToNum3 :: Bool -> Bool -> Bool -> Word8
boolsToNum3 a b c = boolsToNum [a,b,c]

boolsToNum4 :: Bool -> Bool -> Bool -> Bool -> Word8
boolsToNum4 a b c d = boolsToNum [a,b,c,d]

boolsToNum5 :: Bool -> Bool -> Bool -> Bool -> Bool -> Word8
boolsToNum5 a b c d e = boolsToNum [a,b,c,d,e]

boolsToNum6 :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Word8
boolsToNum6 a b c d e f = boolsToNum [a,b,c,d,e,f]

boolsToNum7 :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Word8
boolsToNum7 a b c d e f g = boolsToNum [a,b,c,d,e,f,g]

boolsToNum8 :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Word8
boolsToNum8 a b c d e f g h = boolsToNum [a,b,c,d,e,f,g,h]

bitsOf :: (Num a, Bits a) => a -> [Bit]
bitsOf n = bool (numToBool (n .&. 1)) : bitsOf (n `shiftR` 1)
{-# INLINE bitsOf #-}

numToBool :: (Eq a, Num a) => a -> Bool
numToBool 0 = False
numToBool _ = True
{-# INLINE numToBool #-}

boolsToNum :: (Num a, Bits a) => [Bool] -> a
boolsToNum = foldl' (\n a -> (n `shiftL` 1) .|. boolToNum a) 0
{-# INLINE boolsToNum #-}

boolToNum :: Num a => Bool -> a
boolToNum False = 0
boolToNum True  = 1
{-# INLINE boolToNum #-}

instance Num Bit1 where
  Bit1 a + Bit1 b = Bit1 (xor a b)
  Bit1 a * Bit1 b = Bit1 (a && b)
  Bit1 a - Bit1 b = Bit1 (xor a b)
  negate a = a
  abs a    = a
  signum a = a
  fromInteger = Bit1 . bool . odd

full_adder :: Bit -> Bit -> Bit -> (Bit, Bit)
full_adder a b cin = (s2, c1 || c2)
  where (s1,c1) = half_adder a b
        (s2,c2) = half_adder s1 cin

half_adder :: Bit -> Bit -> (Bit, Bit)
half_adder a b = (a `xor` b, a && b)

instance Num Bit2 where
  Bit2 a2 a1 + Bit2 b2 b1 = Bit2 s2 s1 where
    (s1,c2) = half_adder a1 b1
    (s2,_)  = full_adder a2 b2 c2
  Bit2 a2 a1 * Bit2 b2 b1 = Bit2 ((a1 && b2) `xor` (a2 && b1)) (a1 && b1)
    -- wallace tree
    --
    --   XX
    --  XX
    -- ----
    --  XXX
    --  X
    -- ----
    -- XXXX
    --
    -- But we only need the first 2 bits
  negate (Bit2 a b) = Bit2 (not a) (not b) + 1
  abs a = a
  signum (Bit2 a b) = Bit2 false (a || b)
  fromInteger k = Bit2 (bool (k .&. 2 /= 0)) (bool (k .&. 1 /= 0))
