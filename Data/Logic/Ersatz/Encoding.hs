{-# LANGUAGE TypeFamilies, Rank2Types #-}
module Data.Logic.Ersatz.Encoding
  ( Encoding(..)
  , decodeFunctor
  , decideFoldable
  ) where

import Data.Logic.Ersatz.Solution
import Data.Array
import Data.Ix
import qualified Data.Foldable as Foldable
import Data.Foldable (Foldable)
import Data.Logic.Ersatz.Internal.Problem
import qualified Data.IntMap as IntMap
-- import Control.Applicative

class Encoding a where
  type Decoded a :: *
  -- | Return a value based on the solution, which is included in the model.
  decode  :: Solution -> a -> Decoded a
  -- | Return whether or not the value is determined by the solution
  decide  :: Solution -> a -> Bool

instance Encoding Literal where
  type Decoded Literal = Bool
  decode m l
    | i < 0     = maybe True not $ IntMap.lookup (negate i) m
    | otherwise = maybe False id $ IntMap.lookup i m
    where i = literalId l
  decide m l
    | i < 0     = IntMap.member (negate i) m
    | otherwise = IntMap.member i m
    where i = literalId l

instance Encoding Lit where
  type Decoded Lit = Bool
  decode _ (Bool b) = b
  decode f (Lit l)  = decode f l
  decide _ (Bool _) = True
  decide f (Lit l)  = decide f l

instance (Encoding a, Encoding b) => Encoding (a,b) where
  type Decoded (a,b) = (Decoded a, Decoded b)
  decode f (a,b) =  (decode f a, decode f b)
  decide f (a,b) = decide f a && decide f b

instance (Encoding a, Encoding b, Encoding c) => Encoding (a,b,c) where
  type Decoded (a,b,c) = (Decoded a, Decoded b, Decoded c)
  decode f (a,b,c) = (decode f a, decode f b, decode f c)
  decide f (a,b,c) = decide f a && decide f b && decide f c

instance (Encoding a, Encoding b, Encoding c, Encoding d) => Encoding (a,b,c,d) where
  type Decoded (a,b,c,d) = (Decoded a, Decoded b, Decoded c, Decoded d)
  decode f (a,b,c,d) = (decode f a, decode f b, decode f c, decode f d)
  decide f (a,b,c,d) = decide f a && decide f b && decide f c && decide f d

instance (Encoding a, Encoding b) => Encoding (Either a b) where
  type Decoded (Either a b) = Either (Decoded a) (Decoded b)
  decode f (Left a) = Left (decode f a)
  decode f (Right b) = Right (decode f b)
  decide f (Left a) = decide f a
  decide f (Right b) = decide f b

decodeFunctor :: (Functor f, Encoding a) => Solution -> f a -> f (Decoded a)
decodeFunctor f = fmap (decode f)

decideFoldable :: (Foldable f, Encoding a) => Solution -> f a -> Bool
decideFoldable f = Foldable.all (decide f)

instance Encoding a => Encoding [a] where
  type Decoded [a] = [Decoded a]
  decode = decodeFunctor
  decide = decideFoldable

instance Encoding a => Encoding (Maybe a) where
  type Decoded (Maybe a) = Maybe (Decoded a)
  decode = decodeFunctor
  decide = decideFoldable

instance (Ix i, Encoding e) => Encoding (Array i e) where
  type Decoded (Array i e) = Array i (Decoded e)
  decode = decodeFunctor
  decide = decideFoldable
