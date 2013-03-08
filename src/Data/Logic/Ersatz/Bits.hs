{-# LANGUAGE TypeFamilies #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2010-2013, Johan Kiviniemi 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.Logic.Ersatz.Bits
  ( Bit1(..), Bit2(..), Bit3(..), Bit4(..), Bit5(..), Bit6(..), Bit7(..)
  , Bit8(..)
  , encodeBit1, encodeBit2, encodeBit3, encodeBit4, encodeBit5, encodeBit6
  , encodeBit7, encodeBit8
  ) where

import Prelude hiding ((&&), (||), and, or, not)

import Control.Applicative
import Data.Bits (Bits, (.&.), (.|.), shiftL, shiftR)
import Data.List (foldl', unzip4, unzip5, unzip6, unzip7)
import Data.Word (Word8)

import Data.Logic.Ersatz.Bit
import Data.Logic.Ersatz.Encoding
import Data.Logic.Ersatz.Problem

newtype Bit1 = Bit1 Bit deriving (Show)
data Bit2 = Bit2 Bit Bit deriving (Show)
data Bit3 = Bit3 Bit Bit Bit deriving (Show)
data Bit4 = Bit4 Bit Bit Bit Bit deriving (Show)
data Bit5 = Bit5 Bit Bit Bit Bit Bit deriving (Show)
data Bit6 = Bit6 Bit Bit Bit Bit Bit Bit deriving (Show)
data Bit7 = Bit7 Bit Bit Bit Bit Bit Bit Bit deriving (Show)
data Bit8 = Bit8 Bit Bit Bit Bit Bit Bit Bit Bit deriving (Show)

-- Holy boilerplate, Batman!

instance Boolean Bit1 where
  bool True  = Bit1 true
  bool False = Bit1 false
  true       = Bit1 true
  false      = Bit1 false
  Bit1 a &&  Bit1 a' = Bit1 (a &&  a')
  Bit1 a ||  Bit1 a' = Bit1 (a ||  a')
  Bit1 a ==> Bit1 a' = Bit1 (a ==> a')
  not (Bit1 a) = Bit1 (not a)
  and  = listOp1 and
  or   = listOp1 or
  nand = listOp1 nand
  nor  = listOp1 nor
  xor (Bit1 a) (Bit1 a') = Bit1 (xor a a')
  choose (Bit1 f) (Bit1 t) (Bit1 s) = Bit1 (choose f t s)

instance Boolean Bit2 where
  bool True  = Bit2 true  true
  bool False = Bit2 false false
  true       = Bit2 true  true
  false      = Bit2 false false
  Bit2 a b &&  Bit2 a' b' = Bit2 (a &&  a') (b &&  b')
  Bit2 a b ||  Bit2 a' b' = Bit2 (a ||  a') (b ||  b')
  Bit2 a b ==> Bit2 a' b' = Bit2 (a ==> a') (b ==> b')
  not (Bit2 a b) = Bit2 (not a) (not b)
  and  = listOp2 and
  or   = listOp2 or
  nand = listOp2 nand
  nor  = listOp2 nor
  xor (Bit2 a b) (Bit2 a' b') = Bit2 (xor a a') (xor b b')
  choose (Bit2 fa fb) (Bit2 ta tb) (Bit2 sa sb) = Bit2 (choose fa ta sa) (choose fb tb sb)

instance Boolean Bit3 where
  bool True  = Bit3 true  true  true
  bool False = Bit3 false false false
  true       = Bit3 true  true  true
  false      = Bit3 false false false
  Bit3 a b c &&  Bit3 a' b' c' = Bit3 (a &&  a') (b &&  b') (c &&  c')
  Bit3 a b c ||  Bit3 a' b' c' = Bit3 (a ||  a') (b ||  b') (c ||  c')
  Bit3 a b c ==> Bit3 a' b' c' = Bit3 (a ==> a') (b ==> b') (c ==> c')
  not (Bit3 a b c) = Bit3 (not a) (not b) (not c)
  and  = listOp3 and
  or   = listOp3 or
  nand = listOp3 nand
  nor  = listOp3 nor
  xor (Bit3 a b c) (Bit3 a' b' c') = Bit3 (xor a a') (xor b b') (xor c c')
  choose (Bit3 fa fb fc) (Bit3 ta tb tc) (Bit3 sa sb sc) = Bit3 (choose fa ta sa) (choose fb tb sb) (choose fc tc sc)

instance Boolean Bit4 where
  bool True  = Bit4 true  true  true  true
  bool False = Bit4 false false false false
  true       = Bit4 true  true  true  true
  false      = Bit4 false false false false
  Bit4 a b c d &&  Bit4 a' b' c' d' = Bit4 (a &&  a') (b &&  b') (c &&  c') (d &&  d')
  Bit4 a b c d ||  Bit4 a' b' c' d' = Bit4 (a ||  a') (b ||  b') (c ||  c') (d ||  d')
  Bit4 a b c d ==> Bit4 a' b' c' d' = Bit4 (a ==> a') (b ==> b') (c ==> c') (d ==> d')
  not (Bit4 a b c d) = Bit4 (not a) (not b) (not c) (not d)
  and  = listOp4 and
  or   = listOp4 or
  nand = listOp4 nand
  nor  = listOp4 nor
  xor (Bit4 a b c d) (Bit4 a' b' c' d') = Bit4 (xor a a') (xor b b') (xor c c') (xor d d')
  choose (Bit4 fa fb fc fd) (Bit4 ta tb tc td) (Bit4 sa sb sc sd) = Bit4 (choose fa ta sa) (choose fb tb sb) (choose fc tc sc) (choose fd td sd)

instance Boolean Bit5 where
  bool True  = Bit5 true  true  true  true  true
  bool False = Bit5 false false false false false
  true       = Bit5 true  true  true  true  true
  false      = Bit5 false false false false false
  Bit5 a b c d e &&  Bit5 a' b' c' d' e' = Bit5 (a &&  a') (b &&  b') (c &&  c') (d &&  d') (e &&  e')
  Bit5 a b c d e ||  Bit5 a' b' c' d' e' = Bit5 (a ||  a') (b ||  b') (c ||  c') (d ||  d') (e ||  e')
  Bit5 a b c d e ==> Bit5 a' b' c' d' e' = Bit5 (a ==> a') (b ==> b') (c ==> c') (d ==> d') (e ==> e')
  not (Bit5 a b c d e) = Bit5 (not a) (not b) (not c) (not d) (not e)
  and  = listOp5 and
  or   = listOp5 or
  nand = listOp5 nand
  nor  = listOp5 nor
  xor (Bit5 a b c d e) (Bit5 a' b' c' d' e') = Bit5 (xor a a') (xor b b') (xor c c') (xor d d') (xor e e')
  choose (Bit5 fa fb fc fd fe) (Bit5 ta tb tc td te) (Bit5 sa sb sc sd se) = Bit5 (choose fa ta sa) (choose fb tb sb) (choose fc tc sc) (choose fd td sd) (choose fe te se)

instance Boolean Bit6 where
  bool True  = Bit6 true  true  true  true  true  true
  bool False = Bit6 false false false false false false
  true       = Bit6 true  true  true  true  true  true
  false      = Bit6 false false false false false false
  Bit6 a b c d e f &&  Bit6 a' b' c' d' e' f' = Bit6 (a &&  a') (b &&  b') (c &&  c') (d &&  d') (e &&  e') (f &&  f')
  Bit6 a b c d e f ||  Bit6 a' b' c' d' e' f' = Bit6 (a ||  a') (b ||  b') (c ||  c') (d ||  d') (e ||  e') (f ||  f')
  Bit6 a b c d e f ==> Bit6 a' b' c' d' e' f' = Bit6 (a ==> a') (b ==> b') (c ==> c') (d ==> d') (e ==> e') (f ==> f')
  not (Bit6 a b c d e f) = Bit6 (not a) (not b) (not c) (not d) (not e) (not f)
  and  = listOp6 and
  or   = listOp6 or
  nand = listOp6 nand
  nor  = listOp6 nor
  xor (Bit6 a b c d e f) (Bit6 a' b' c' d' e' f') = Bit6 (xor a a') (xor b b') (xor c c') (xor d d') (xor e e') (xor f f')
  choose (Bit6 fa fb fc fd fe ff) (Bit6 ta tb tc td te tf) (Bit6 sa sb sc sd se sf) = Bit6 (choose fa ta sa) (choose fb tb sb) (choose fc tc sc) (choose fd td sd) (choose fe te se) (choose ff tf sf)

instance Boolean Bit7 where
  bool True  = Bit7 true  true  true  true  true  true  true
  bool False = Bit7 false false false false false false false
  true       = Bit7 true  true  true  true  true  true  true
  false      = Bit7 false false false false false false false
  Bit7 a b c d e f g &&  Bit7 a' b' c' d' e' f' g' = Bit7 (a &&  a') (b &&  b') (c &&  c') (d &&  d') (e &&  e') (f &&  f') (g &&  g')
  Bit7 a b c d e f g ||  Bit7 a' b' c' d' e' f' g' = Bit7 (a ||  a') (b ||  b') (c ||  c') (d ||  d') (e ||  e') (f ||  f') (g ||  g')
  Bit7 a b c d e f g ==> Bit7 a' b' c' d' e' f' g' = Bit7 (a ==> a') (b ==> b') (c ==> c') (d ==> d') (e ==> e') (f ==> f') (g ==> g')
  not (Bit7 a b c d e f g) = Bit7 (not a) (not b) (not c) (not d) (not e) (not f) (not g)
  and  = listOp7 and
  or   = listOp7 or
  nand = listOp7 nand
  nor  = listOp7 nor
  xor (Bit7 a b c d e f g) (Bit7 a' b' c' d' e' f' g') = Bit7 (xor a a') (xor b b') (xor c c') (xor d d') (xor e e') (xor f f') (xor g g')
  choose (Bit7 fa fb fc fd fe ff fg) (Bit7 ta tb tc td te tf tg) (Bit7 sa sb sc sd se sf sg) = Bit7 (choose fa ta sa) (choose fb tb sb) (choose fc tc sc) (choose fd td sd) (choose fe te se) (choose ff tf sf) (choose fg tg sg)

instance Boolean Bit8 where
  bool True  = Bit8 true  true  true  true  true  true  true  true
  bool False = Bit8 false false false false false false false false
  true       = Bit8 true  true  true  true  true  true  true  true
  false      = Bit8 false false false false false false false false
  Bit8 a b c d e f g h &&  Bit8 a' b' c' d' e' f' g' h' = Bit8 (a &&  a') (b &&  b') (c &&  c') (d &&  d') (e &&  e') (f &&  f') (g &&  g') (h &&  h')
  Bit8 a b c d e f g h ||  Bit8 a' b' c' d' e' f' g' h' = Bit8 (a ||  a') (b ||  b') (c ||  c') (d ||  d') (e ||  e') (f ||  f') (g ||  g') (h ||  h')
  Bit8 a b c d e f g h ==> Bit8 a' b' c' d' e' f' g' h' = Bit8 (a ==> a') (b ==> b') (c ==> c') (d ==> d') (e ==> e') (f ==> f') (g ==> g') (h ==> h')
  not (Bit8 a b c d e f g h) = Bit8 (not a) (not b) (not c) (not d) (not e) (not f) (not g) (not h)
  and  = listOp8 and
  or   = listOp8 or
  nand = listOp8 nand
  nor  = listOp8 nor
  xor (Bit8 a b c d e f g h) (Bit8 a' b' c' d' e' f' g' h') = Bit8 (xor a a') (xor b b') (xor c c') (xor d d') (xor e e') (xor f f') (xor g g') (xor h h')
  choose (Bit8 fa fb fc fd fe ff fg fh) (Bit8 ta tb tc td te tf tg th) (Bit8 sa sb sc sd se sf sg sh) = Bit8 (choose fa ta sa) (choose fb tb sb) (choose fc tc sc) (choose fd td sd) (choose fe te se) (choose ff tf sf) (choose fg tg sg) (choose fh th sh)

instance Equatable Bit1 where
  Bit1 a === Bit1 a' = a === a'
  Bit1 a /== Bit1 a' = a /== a'

instance Equatable Bit2 where
  Bit2 a b === Bit2 a' b' = a === a' && b === b'
  Bit2 a b /== Bit2 a' b' = a /== a' || b /== b'

instance Equatable Bit3 where
  Bit3 a b c === Bit3 a' b' c' = a === a' && b === b' && c === c'
  Bit3 a b c /== Bit3 a' b' c' = a /== a' || b /== b' || c /== c'

instance Equatable Bit4 where
  Bit4 a b c d === Bit4 a' b' c' d' = a === a' && b === b' && c === c' && d === d'
  Bit4 a b c d /== Bit4 a' b' c' d' = a /== a' || b /== b' || c /== c' || d /== d'

instance Equatable Bit5 where
  Bit5 a b c d e === Bit5 a' b' c' d' e' = a === a' && b === b' && c === c' && d === d' && e === e'
  Bit5 a b c d e /== Bit5 a' b' c' d' e' = a /== a' || b /== b' || c /== c' || d /== d' || e /== e'

instance Equatable Bit6 where
  Bit6 a b c d e f === Bit6 a' b' c' d' e' f' = a === a' && b === b' && c === c' && d === d' && e === e' && f === f'
  Bit6 a b c d e f /== Bit6 a' b' c' d' e' f' = a /== a' || b /== b' || c /== c' || d /== d' || e /== e' || f /== f'

instance Equatable Bit7 where
  Bit7 a b c d e f g === Bit7 a' b' c' d' e' f' g' = a === a' && b === b' && c === c' && d === d' && e === e' && f === f' && g === g'
  Bit7 a b c d e f g /== Bit7 a' b' c' d' e' f' g' = a /== a' || b /== b' || c /== c' || d /== d' || e /== e' || f /== f' || g /== g'

instance Equatable Bit8 where
  Bit8 a b c d e f g h === Bit8 a' b' c' d' e' f' g' h' = a === a' && b === b' && c === c' && d === d' && e === e' && f === f' && g === g' && h === h'
  Bit8 a b c d e f g h /== Bit8 a' b' c' d' e' f' g' h' = a /== a' || b /== b' || c /== c' || d /== d' || e /== e' || f /== f' || g /== g' || h /== h'

instance Variable Bit1 where
  exists = Bit1 <$> exists
  forall = Bit1 <$> forall

instance Variable Bit2 where
  exists = Bit2 <$> exists <*> exists
  forall = Bit2 <$> forall <*> forall

instance Variable Bit3 where
  exists = Bit3 <$> exists <*> exists <*> exists
  forall = Bit3 <$> forall <*> forall <*> forall

instance Variable Bit4 where
  exists = Bit4 <$> exists <*> exists <*> exists <*> exists
  forall = Bit4 <$> forall <*> forall <*> forall <*> forall

instance Variable Bit5 where
  exists = Bit5 <$> exists <*> exists <*> exists <*> exists <*> exists
  forall = Bit5 <$> forall <*> forall <*> forall <*> forall <*> forall

instance Variable Bit6 where
  exists = Bit6 <$> exists <*> exists <*> exists <*> exists <*> exists <*> exists
  forall = Bit6 <$> forall <*> forall <*> forall <*> forall <*> forall <*> forall

instance Variable Bit7 where
  exists = Bit7 <$> exists <*> exists <*> exists <*> exists <*> exists <*> exists <*> exists
  forall = Bit7 <$> forall <*> forall <*> forall <*> forall <*> forall <*> forall <*> forall

instance Variable Bit8 where
  exists = Bit8 <$> exists <*> exists <*> exists <*> exists <*> exists <*> exists <*> exists <*> exists
  forall = Bit8 <$> forall <*> forall <*> forall <*> forall <*> forall <*> forall <*> forall <*> forall

instance Decode Bit1 where
  type Decoded Bit1 = Word8
  decode s (Bit1 a) = fmap boolsToNum1 <$> decode s a

instance Decode Bit2 where
  type Decoded Bit2 = Word8
  decode s (Bit2 a b) = go <$> decode s a <*> decode s b
    where go a' b' = boolsToNum2 <$> a' <*> b'

instance Decode Bit3 where
  type Decoded Bit3 = Word8
  decode s (Bit3 a b c) = go <$> decode s a <*> decode s b <*> decode s c
    where go a' b' c' = boolsToNum3 <$> a' <*> b' <*> c'

instance Decode Bit4 where
  type Decoded Bit4 = Word8
  decode s (Bit4 a b c d) = go <$> decode s a <*> decode s b <*> decode s c <*> decode s d
    where go a' b' c' d' = boolsToNum4 <$> a' <*> b' <*> c' <*> d'

instance Decode Bit5 where
  type Decoded Bit5 = Word8
  decode s (Bit5 a b c d e) = go <$> decode s a <*> decode s b <*> decode s c <*> decode s d <*> decode s e
    where go a' b' c' d' e' = boolsToNum5 <$> a' <*> b' <*> c' <*> d' <*> e'

instance Decode Bit6 where
  type Decoded Bit6 = Word8
  decode s (Bit6 a b c d e f) = go <$> decode s a <*> decode s b <*> decode s c <*> decode s d <*> decode s e <*> decode s f
    where go a' b' c' d' e' f' = boolsToNum6 <$> a' <*> b' <*> c' <*> d' <*> e' <*> f'

instance Decode Bit7 where
  type Decoded Bit7 = Word8
  decode s (Bit7 a b c d e f g) = go <$> decode s a <*> decode s b <*> decode s c <*> decode s d <*> decode s e <*> decode s f <*> decode s g
    where go a' b' c' d' e' f' g' = boolsToNum7 <$> a' <*> b' <*> c' <*> d' <*> e' <*> f' <*> g'

instance Decode Bit8 where
  type Decoded Bit8 = Word8
  decode s (Bit8 a b c d e f g h) = go <$> decode s a <*> decode s b <*> decode s c <*> decode s d <*> decode s e <*> decode s f <*> decode s g <*> decode s h
    where go a' b' c' d' e' f' g' h' = boolsToNum8 <$> a' <*> b' <*> c' <*> d' <*> e' <*> f' <*> g' <*> h'

instance Encode Bit1 where
  type Encoded Bit1 = Word8
  encode = encodeBit1

instance Encode Bit2 where
  type Encoded Bit2 = Word8
  encode = encodeBit2

instance Encode Bit3 where
  type Encoded Bit3 = Word8
  encode = encodeBit3

instance Encode Bit4 where
  type Encoded Bit4 = Word8
  encode = encodeBit4

instance Encode Bit5 where
  type Encoded Bit5 = Word8
  encode = encodeBit5

instance Encode Bit6 where
  type Encoded Bit6 = Word8
  encode = encodeBit6

instance Encode Bit7 where
  type Encoded Bit7 = Word8
  encode = encodeBit7

instance Encode Bit8 where
  type Encoded Bit8 = Word8
  encode = encodeBit8

listOp1 :: ([Bit] -> Bit) -> [Bit1] -> Bit1
listOp1 op = Bit1 . op . map (\(Bit1 a) -> a)

listOp2 :: ([Bit] -> Bit) -> [Bit2] -> Bit2
listOp2 op = (\(as,bs) -> Bit2 (op as) (op bs))
           . unzip . map (\(Bit2 a b) -> (a,b))

listOp3 :: ([Bit] -> Bit) -> [Bit3] -> Bit3
listOp3 op = (\(as,bs,cs) -> Bit3 (op as) (op bs) (op cs))
           . unzip3 . map (\(Bit3 a b c) -> (a,b,c))

listOp4 :: ([Bit] -> Bit) -> [Bit4] -> Bit4
listOp4 op = (\(as,bs,cs,ds) -> Bit4 (op as) (op bs) (op cs) (op ds))
           . unzip4 . map (\(Bit4 a b c d) -> (a,b,c,d))

listOp5 :: ([Bit] -> Bit) -> [Bit5] -> Bit5
listOp5 op = (\(as,bs,cs,ds,es) -> Bit5 (op as) (op bs) (op cs) (op ds) (op es))
           . unzip5 . map (\(Bit5 a b c d e) -> (a,b,c,d,e))

listOp6 :: ([Bit] -> Bit) -> [Bit6] -> Bit6
listOp6 op = (\(as,bs,cs,ds,es,fs) -> Bit6 (op as) (op bs) (op cs) (op ds) (op es) (op fs))
           . unzip6 . map (\(Bit6 a b c d e f) -> (a,b,c,d,e,f))

listOp7 :: ([Bit] -> Bit) -> [Bit7] -> Bit7
listOp7 op = (\(as,bs,cs,ds,es,fs,gs) -> Bit7 (op as) (op bs) (op cs) (op ds) (op es) (op fs) (op gs))
           . unzip7 . map (\(Bit7 a b c d e f g) -> (a,b,c,d,e,f,g))

listOp8 :: ([Bit] -> Bit) -> [Bit8] -> Bit8
listOp8 op = (\(as,bs,cs,ds,es,fs,gs,hs) -> Bit8 (op as) (op bs) (op cs) (op ds) (op es) (op fs) (op gs) (op hs))
           . unzip8 . map (\(Bit8 a b c d e f g h) -> (a,b,c,d,e,f,g,h))
  where
    unzip8 = foldr (\(a,b,c,d,e,f,g,h) ~(as,bs,cs,ds,es,fs,gs,hs) ->
                       (a:as,b:bs,c:cs,d:ds,e:es,f:fs,g:gs,h:hs))
                   ([],[],[],[],[],[],[],[])

encodeBit1 :: Word8 -> Bit1
encodeBit1 i = Bit1 a where (a:_) = bitsOf i

encodeBit2 :: Word8 -> Bit2
encodeBit2 i = Bit2 a b where (b:a:_) = bitsOf i

encodeBit3 :: Word8 -> Bit3
encodeBit3 i = Bit3 a b c where (c:b:a:_) = bitsOf i

encodeBit4 :: Word8 -> Bit4
encodeBit4 i = Bit4 a b c d where (d:c:b:a:_) = bitsOf i

encodeBit5 :: Word8 -> Bit5
encodeBit5 i = Bit5 a b c d e where (e:d:c:b:a:_) = bitsOf i

encodeBit6 :: Word8 -> Bit6
encodeBit6 i = Bit6 a b c d e f where (f:e:d:c:b:a:_) = bitsOf i

encodeBit7 :: Word8 -> Bit7
encodeBit7 i = Bit7 a b c d e f g where (g:f:e:d:c:b:a:_) = bitsOf i

encodeBit8 :: Word8 -> Bit8
encodeBit8 i = Bit8 a b c d e f g h where (h:g:f:e:d:c:b:a:_) = bitsOf i

boolsToNum1 :: Bool -> Word8
boolsToNum1 a = boolToNum a

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
