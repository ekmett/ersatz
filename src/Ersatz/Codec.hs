{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
--------------------------------------------------------------------
-- |
-- Copyright :  © Edward Kmett 2010-2014, Johan Kiviniemi 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ersatz.Codec
  ( Codec(..)
  ) where

import Control.Applicative
import Control.Monad hiding (mapM)
import Data.Array
import Data.HashMap.Lazy (HashMap)
import Data.IntMap (IntMap)
import Data.Kind
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Traversable
import Data.Tree (Tree)
import Ersatz.Internal.Literal
import Ersatz.Solution
import Prelude hiding (mapM)

-- | This class describes data types that can be marshaled to or from a SAT solver.
class Codec a where
  type Decoded a :: Type
  -- | Return 'Just' a value based on the solution if one can be determined.
  -- Otherwise, return 'Nothing'.
  decode :: Solution -> a -> Maybe (Decoded a)
  encode :: Decoded a -> a

-- | By convention, the 'decode' implementation will return 'False' for
-- unconstrained non-negative 'Literal's and 'True' for unconstrained negative
-- 'Literal's.
instance Codec Literal where
  type Decoded Literal = Bool
  decode s a = case solutionLiteral s a of
                 sol@(Just _) -> sol
                 Nothing
                   | i >= 0    -> Just False
                   | otherwise -> Just True
    where
      i = literalId a
  encode True  = literalTrue
  encode False = literalFalse

instance Codec () where
  type Decoded () = ()
  decode _ () = pure ()
  encode   () = ()

instance (Codec a, Codec b) => Codec (a,b) where
  type Decoded (a,b) = (Decoded a, Decoded b)
  decode s (a,b) = (,) <$> decode s a <*> decode s b
  encode   (a,b) = (encode a, encode b)

instance (Codec a, Codec b, Codec c) => Codec (a,b,c) where
  type Decoded (a,b,c) = (Decoded a, Decoded b, Decoded c)
  decode s (a,b,c) = (,,) <$> decode s a <*> decode s b <*> decode s c
  encode   (a,b,c) = (encode a, encode b, encode c)

instance (Codec a, Codec b, Codec c, Codec d) => Codec (a,b,c,d) where
  type Decoded (a,b,c,d) = (Decoded a, Decoded b, Decoded c, Decoded d)
  decode s (a,b,c,d) = (,,,) <$> decode s a <*> decode s b <*> decode s c <*> decode s d
  encode   (a,b,c,d) = (encode a, encode b, encode c, encode d)

instance (Codec a, Codec b, Codec c, Codec d, Codec e) => Codec (a,b,c,d,e) where
  type Decoded (a,b,c,d,e) = (Decoded a, Decoded b, Decoded c, Decoded d, Decoded e)
  decode s (a,b,c,d,e) = (,,,,) <$> decode s a <*> decode s b <*> decode s c <*> decode s d <*> decode s e
  encode   (a,b,c,d,e) = (encode a, encode b, encode c, encode d, encode e)

instance (Codec a, Codec b, Codec c, Codec d, Codec e, Codec f) => Codec (a,b,c,d,e,f) where
  type Decoded (a,b,c,d,e,f) = (Decoded a, Decoded b, Decoded c, Decoded d, Decoded e, Decoded f)
  decode s (a,b,c,d,e,f) = (,,,,,) <$> decode s a <*> decode s b <*> decode s c <*> decode s d <*> decode s e <*> decode s f
  encode   (a,b,c,d,e,f) = (encode a, encode b, encode c, encode d, encode e, encode f)

instance (Codec a, Codec b, Codec c, Codec d, Codec e, Codec f, Codec g) => Codec (a,b,c,d,e,f,g) where
  type Decoded (a,b,c,d,e,f,g) = (Decoded a, Decoded b, Decoded c, Decoded d, Decoded e, Decoded f, Decoded g)
  decode s (a,b,c,d,e,f,g) = (,,,,,,) <$> decode s a <*> decode s b <*> decode s c <*> decode s d <*> decode s e <*> decode s f <*> decode s g
  encode   (a,b,c,d,e,f,g) = (encode a, encode b, encode c, encode d, encode e, encode f, encode g)

instance (Codec a, Codec b, Codec c, Codec d, Codec e, Codec f, Codec g, Codec h) => Codec (a,b,c,d,e,f,g,h) where
  type Decoded (a,b,c,d,e,f,g,h) = (Decoded a, Decoded b, Decoded c, Decoded d, Decoded e, Decoded f, Decoded g, Decoded h)
  decode s (a,b,c,d,e,f,g,h) = (,,,,,,,) <$> decode s a <*> decode s b <*> decode s c <*> decode s d <*> decode s e <*> decode s f <*> decode s g <*> decode s h
  encode   (a,b,c,d,e,f,g,h) = (encode a, encode b, encode c, encode d, encode e, encode f, encode g, encode h)

instance Codec a => Codec [a] where
  type Decoded [a] = [Decoded a]
  decode = mapM . decode
  encode = map encode

instance (Ix i, Codec e) => Codec (Array i e) where
  type Decoded (Array i e) = Array i (Decoded e)
  decode = mapM . decode
  encode = fmap encode

instance (Codec a, Codec b) => Codec (Either a b) where
  type Decoded (Either a b) = Either (Decoded a) (Decoded b)
  decode s (Left  a) = Left  <$> decode s a
  decode s (Right b) = Right <$> decode s b
  encode   (Left  a) = Left  (encode a)
  encode   (Right b) = Right (encode b)

instance Codec a => Codec (HashMap k a) where
  type Decoded (HashMap k a) = HashMap k (Decoded a)
  decode = mapM . decode
  encode = fmap encode

instance Codec a => Codec (IntMap a) where
  type Decoded (IntMap a) = IntMap (Decoded a)
  decode = mapM . decode
  encode = fmap encode

instance Codec a => Codec (Map k a) where
  type Decoded (Map k a) = Map k (Decoded a)
  decode = mapM . decode
  encode = fmap encode

instance Codec a => Codec (Maybe a) where
  type Decoded (Maybe a) = Maybe (Decoded a)
  decode = mapM . decode
  encode = fmap encode

instance Codec a => Codec (Seq a) where
  type Decoded (Seq a) = Seq (Decoded a)
  decode = mapM . decode
  encode = fmap encode

instance Codec a => Codec (Tree a) where
  type Decoded (Tree a) = Tree (Decoded a)
  decode = mapM . decode
  encode = fmap encode
