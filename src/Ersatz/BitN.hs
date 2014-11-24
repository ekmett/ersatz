{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
--------------------------------------------------------------------
-- |
-- Copyright :  © Eric Mertens 2014
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ersatz.BitN
  ( BitN(..)
  , FromFixed(..)
  , isEven
  , isOdd
  , sumBit
  , sumBitN
  ) where

import Control.Monad.Trans.State (State, runState, get, put)
import Data.Foldable (Foldable, toList)
import Data.List (unfoldr)
import Data.Traversable (traverse)
import Data.Typeable (Typeable)
import Prelude hiding (and, or, not, (&&), (||))

import Ersatz

-- | 'BitN' is an arbitrary length natural number type
-- suitable for comparisons and arithmetic. Bits are stored
-- in little-endian order to enable phantom 'false' values
-- to be truncated.
newtype BitN = BitN [Bit]
  deriving (Show,Typeable)

instance Equatable BitN where
  BitN xs === BitN ys = and (zipWithBitN (===) xs ys)
  BitN xs /== BitN ys = or  (zipWithBitN (/==) xs ys)

-- | Zip the component bits of a 'BitN' extending the
-- shorter argument with 'false' values.
zipWithBitN :: (Bit -> Bit -> a) -> [Bit] -> [Bit] -> [a]
zipWithBitN _ []     []     = []
zipWithBitN f (x:xs) (y:ys) = f x y : zipWithBitN f xs ys
zipWithBitN f xs     []     = map (`f` false) xs
zipWithBitN f []     ys     = map (false `f`) ys

instance Orderable BitN where
  BitN xs <?  BitN ys = orderHelper false xs ys
  BitN xs <=? BitN ys = orderHelper true  xs ys

orderHelper :: Bit -> [Bit] -> [Bit] -> Bit
orderHelper c0 xs ys = foldl aux c0 (zipWithBitN (,) xs ys)
    where
    aux c (x,y) = c && x === y || x <? y

instance Codec BitN where
  type Decoded BitN = Integer

  decode s (BitN xs) =
    do ys <- traverse (decode s) xs
       -- bools to Integers
       let zs = map (\x -> if x then 1 else 0) ys
       -- Integers to Integer
       return (foldr (\x acc -> x + 2 * acc) 0 zs)

  encode = BitN . unfoldr step
    where
    step x =
      case compare x 0 of
        LT -> error "BitN/encode: Negative number"
        EQ -> Nothing
        GT -> Just (if odd x then true else false, x `div` 2)

-- | Add two 'BitN' values given an incoming carry bit.
addBitN :: Bit -> BitN -> BitN -> BitN
addBitN c (BitN xs0) (BitN ys0) = BitN (add2 c xs0 ys0)
  where
  add2 cin []     ys    = add1 cin ys
  add2 cin xs     []    = add1 cin xs
  add2 cin (x:xs) (y:ys)= s : add2 cout xs ys
    where
    (s,cout)            = fullAdder x y cin

  add1 cin []           = [cin]
  add1 cin (x:xs)       = s : add1 cout xs
    where
    (s,cout)            = halfAdder cin x

-- | Compute the sum of a source of 'BitN' values.
sumBitN :: Foldable t => t BitN -> BitN
sumBitN = sumBitN' . toList

sumBitN' :: [BitN] -> BitN
sumBitN' []  = BitN []
sumBitN' [x] = x
sumBitN' xs0 = sumBitN (merge xs0)
  where
  merge [x] = [x]
  merge []  = []
  merge (x1:x2:xs) = addBitN false x1 x2 : merge xs

-- | Optimization of 'sumBitN' enabled when summing
-- individual 'Bit's.
sumBit :: Foldable t => t Bit -> BitN
sumBit t =
  case runState (merge (map fromFixed h2)) h1 of
    (s,[]) -> s
    _      -> error "BitN.betterSumBitN: OOPS! Bad algorithm!"

  where
  ts = toList t
  (h1,h2) = splitAt ((length ts-1) `div` 2) ts

  spareBit = do
    xs <- get
    case xs of
      []   -> return false
      y:ys -> put ys >> return y

  merge :: [BitN] -> State [Bit] BitN
  merge [x] = return x
  merge []  = return (BitN [])
  merge xs  = merge =<< merge' xs

  merge' :: [BitN] -> State [Bit] [BitN]
  merge' []  = return []
  merge' [x] = return [x]
  merge' (x1:x2:xs) =
    do cin <- spareBit
       xs' <- merge' xs
       return (addBitN cin x1 x2 : xs')

-- | Predicate for odd-valued 'BitN's.
isOdd :: BitN -> Bit
isOdd (BitN []   ) = false
isOdd (BitN (x:_)) = x

-- | Predicate for even-valued 'BitN's.
isEven :: BitN -> Bit
isEven = not . isOdd

-- | 'FromFixed' provides the 'fromFixed' method for embedding
-- fixed with numeric encoding types into the arbitrary width
-- 'BitN' type.
class FromFixed a where
  fromFixed :: a -> BitN

instance FromFixed Bit where
  fromFixed x = BitN [x]

instance FromFixed Bit1 where
  fromFixed (Bit1 x0) = BitN [x0]

instance FromFixed Bit2 where
  fromFixed (Bit2 x1 x0) = BitN [x0,x1]

instance FromFixed Bit3 where
  fromFixed (Bit3 x2 x1 x0) = BitN [x0,x1,x2]

instance FromFixed Bit4 where
  fromFixed (Bit4 x3 x2 x1 x0) = BitN [x0,x1,x2,x3]

instance FromFixed Bit5 where
  fromFixed (Bit5 x4 x3 x2 x1 x0) = BitN [x0,x1,x2,x3,x4]

instance FromFixed Bit6 where
  fromFixed (Bit6 x5 x4 x3 x2 x1 x0) = BitN [x0,x1,x2,x3,x4,x5]

instance FromFixed Bit7 where
  fromFixed (Bit7 x6 x5 x4 x3 x2 x1 x0) = BitN [x0,x1,x2,x3,x4,x5,x6]

instance FromFixed Bit8 where
  fromFixed (Bit8 x7 x6 x5 x4 x3 x2 x1 x0) = BitN [x0,x1,x2,x3,x4,x5,x6,x7]


mulBitN :: BitN -> BitN -> BitN
mulBitN (BitN xs) (BitN ys0) = sumBitN
                             $ zipWith aux xs (iterate times2 ys0)
  where
  times2 = (false:)
  aux x ys = BitN (map (x &&) ys)

instance Num BitN where
  (+) = addBitN false
  (*) = mulBitN
  (-) = subBitN
  fromInteger = encode
  signum (BitN xs) = BitN [or xs]
  abs x = x


fullSubtract :: Bit -> Bit -> Bit -> (Bit,Bit)
fullSubtract c x y =
  (x `xor` y `xor` c, x && y && c || not x && y || not x && c)

subBitN :: BitN -> BitN -> BitN
subBitN (BitN xs0) (BitN ys0) = BitN (map (not cN &&) ss)
  where
  (cN, ss) = aux false xs0 ys0

  aux c [] [] = (c, [])

  aux c (x:xs) (y:ys) = fmap (z :) (aux cout xs ys)
    where
    (z,cout) = fullSubtract c x y

  aux c [] ys = aux c [false] ys
  aux c xs [] = aux c xs      [false]
