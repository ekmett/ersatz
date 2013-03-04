{-# LANGUAGE TypeFamilies, Rank2Types #-}
module Data.Logic.Ersatz.Encoding
  ( Encoding(..)
  , decodeTraversable
  ) where

import Data.Logic.Ersatz.Solution
import Data.Array
import Data.Logic.Ersatz.Internal.Problem
import qualified Data.IntMap as IntMap
import Data.Traversable (Traversable, traverse)
import Control.Applicative

class Encoding a where
  type Decoded a :: *
  -- | Return a value based on the solution if one can be determined.
  decode :: Solution -> a -> Maybe (Decoded a)

instance Encoding Literal where
  type Decoded Literal = Bool
  decode m l  =  True  <$ IntMap.lookup i m
             <|> False <$ IntMap.lookup (negate i) m
    where i = literalId l

instance Encoding Lit where
  type Decoded Lit = Bool
  decode _ (Bool b) = Just b
  decode f (Lit l)  = decode f l

instance (Encoding a, Encoding b) => Encoding (a,b) where
  type Decoded (a,b) = (Decoded a, Decoded b)
  decode f (a,b) = (,) <$> decode f a <*> decode f b

instance (Encoding a, Encoding b, Encoding c) => Encoding (a,b,c) where
  type Decoded (a,b,c) = (Decoded a, Decoded b, Decoded c)
  decode f (a,b,c) = (,,) <$> decode f a <*> decode f b <*> decode f c

instance (Encoding a, Encoding b, Encoding c, Encoding d) => Encoding (a,b,c,d) where
  type Decoded (a,b,c,d) = (Decoded a, Decoded b, Decoded c, Decoded d)
  decode f (a,b,c,d) = (,,,) <$> decode f a <*> decode f b <*> decode f c <*> decode f d

instance (Encoding a, Encoding b) => Encoding (Either a b) where
  type Decoded (Either a b) = Either (Decoded a) (Decoded b)
  decode f (Left  a) = Left  <$> decode f a
  decode f (Right b) = Right <$> decode f b

decodeTraversable :: (Traversable f, Encoding a) => Solution -> f a -> Maybe (f (Decoded a))
decodeTraversable f = traverse (decode f)

instance Encoding a => Encoding [a] where
  type Decoded [a] = [Decoded a]
  decode = decodeTraversable

instance Encoding a => Encoding (Maybe a) where
  type Decoded (Maybe a) = Maybe (Decoded a)
  decode = decodeTraversable

instance (Ix i, Encoding e) => Encoding (Array i e) where
  type Decoded (Array i e) = Array i (Decoded e)
  decode = decodeTraversable
