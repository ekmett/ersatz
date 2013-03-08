{-# LANGUAGE TypeFamilies, Rank2Types #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2010-2013, Johan Kiviniemi 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.Logic.Ersatz.Encoding
  ( Decoding(..)
  , decodeTraversable
  , Encoding(..)
  , encodeFunctor
  ) where

import Control.Applicative
import Data.Array
import Data.HashMap.Lazy (HashMap)
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Traversable
import Data.Tree (Tree)

import Data.Logic.Ersatz.Internal.Problem
import Data.Logic.Ersatz.Solution

class Decoding a where
  type Decoded a :: *
  -- | Return a value based on the solution if one can be determined.
  decode :: Solution -> a -> Maybe (Decoded a)

instance Decoding Literal where
  type Decoded Literal = Bool
  decode s l = solutionLiteral s l

instance Decoding Lit where
  type Decoded Lit = Bool
  decode _ (Bool b) = Just b
  decode s (Lit l)  = decode s l

instance Decoding () where
  type Decoded () = ()
  decode _ () = Just ()

instance (Decoding a, Decoding b) => Decoding (a,b) where
  type Decoded (a,b) = (Decoded a, Decoded b)
  decode s (a,b) = (,) <$> decode s a <*> decode s b

instance (Decoding a, Decoding b, Decoding c) => Decoding (a,b,c) where
  type Decoded (a,b,c) = (Decoded a, Decoded b, Decoded c)
  decode s (a,b,c) = (,,) <$> decode s a <*> decode s b <*> decode s c

instance (Decoding a, Decoding b, Decoding c, Decoding d) => Decoding (a,b,c,d) where
  type Decoded (a,b,c,d) = (Decoded a, Decoded b, Decoded c, Decoded d)
  decode s (a,b,c,d) = (,,,) <$> decode s a <*> decode s b <*> decode s c <*> decode s d

instance (Decoding a, Decoding b, Decoding c, Decoding d, Decoding e) => Decoding (a,b,c,d,e) where
  type Decoded (a,b,c,d,e) = (Decoded a, Decoded b, Decoded c, Decoded d, Decoded e)
  decode s (a,b,c,d,e) = (,,,,) <$> decode s a <*> decode s b <*> decode s c <*> decode s d <*> decode s e

instance (Decoding a, Decoding b, Decoding c, Decoding d, Decoding e, Decoding f) => Decoding (a,b,c,d,e,f) where
  type Decoded (a,b,c,d,e,f) = (Decoded a, Decoded b, Decoded c, Decoded d, Decoded e, Decoded f)
  decode s (a,b,c,d,e,f) = (,,,,,) <$> decode s a <*> decode s b <*> decode s c <*> decode s d <*> decode s e <*> decode s f

instance (Decoding a, Decoding b, Decoding c, Decoding d, Decoding e, Decoding f, Decoding g) => Decoding (a,b,c,d,e,f,g) where
  type Decoded (a,b,c,d,e,f,g) = (Decoded a, Decoded b, Decoded c, Decoded d, Decoded e, Decoded f, Decoded g)
  decode s (a,b,c,d,e,f,g) = (,,,,,,) <$> decode s a <*> decode s b <*> decode s c <*> decode s d <*> decode s e <*> decode s f <*> decode s g

instance (Decoding a, Decoding b, Decoding c, Decoding d, Decoding e, Decoding f, Decoding g, Decoding h) => Decoding (a,b,c,d,e,f,g,h) where
  type Decoded (a,b,c,d,e,f,g,h) = (Decoded a, Decoded b, Decoded c, Decoded d, Decoded e, Decoded f, Decoded g, Decoded h)
  decode s (a,b,c,d,e,f,g,h) = (,,,,,,,) <$> decode s a <*> decode s b <*> decode s c <*> decode s d <*> decode s e <*> decode s f <*> decode s g <*> decode s h

instance Decoding a => Decoding [a] where
  type Decoded [a] = [Decoded a]
  decode = decodeTraversable

instance (Ix i, Decoding e) => Decoding (Array i e) where
  type Decoded (Array i e) = Array i (Decoded e)
  decode = decodeTraversable

instance (Decoding a, Decoding b) => Decoding (Either a b) where
  type Decoded (Either a b) = Either (Decoded a) (Decoded b)
  decode s (Left  a) = Left  <$> decode s a
  decode s (Right b) = Right <$> decode s b

instance Decoding a => Decoding (HashMap k a) where
  type Decoded (HashMap k a) = HashMap k (Decoded a)
  decode = decodeTraversable

instance Decoding a => Decoding (IntMap a) where
  type Decoded (IntMap a) = IntMap (Decoded a)
  decode = decodeTraversable

instance Decoding a => Decoding (Map k a) where
  type Decoded (Map k a) = Map k (Decoded a)
  decode = decodeTraversable

instance Decoding a => Decoding (Maybe a) where
  type Decoded (Maybe a) = Maybe (Decoded a)
  decode = decodeTraversable

instance Decoding a => Decoding (Seq a) where
  type Decoded (Seq a) = Seq (Decoded a)
  decode = decodeTraversable

instance Decoding a => Decoding (Tree a) where
  type Decoded (Tree a) = Tree (Decoded a)
  decode = decodeTraversable

decodeTraversable :: (Traversable f, Decoding a) => Solution -> f a -> Maybe (f (Decoded a))
decodeTraversable s = traverse (decode s)

class Encoding a where
  type Encoded a :: *
  encode :: Encoded a -> a

instance Encoding Lit where
  type Encoded Lit = Bool
  encode = Bool

instance Encoding () where
  type Encoded () = ()
  encode () = ()

instance (Encoding a, Encoding b) => Encoding (a,b) where
  type Encoded (a,b) = (Encoded a, Encoded b)
  encode (a,b) = (encode a, encode b)

instance (Encoding a, Encoding b, Encoding c) => Encoding (a,b,c) where
  type Encoded (a,b,c) = (Encoded a, Encoded b, Encoded c)
  encode (a,b,c) = (encode a, encode b, encode c)

instance (Encoding a, Encoding b, Encoding c, Encoding d) => Encoding (a,b,c,d) where
  type Encoded (a,b,c,d) = (Encoded a, Encoded b, Encoded c, Encoded d)
  encode (a,b,c,d) = (encode a, encode b, encode c, encode d)

instance (Encoding a, Encoding b, Encoding c, Encoding d, Encoding e) => Encoding (a,b,c,d,e) where
  type Encoded (a,b,c,d,e) = (Encoded a, Encoded b, Encoded c, Encoded d, Encoded e)
  encode (a,b,c,d,e) = (encode a, encode b, encode c, encode d, encode e)

instance (Encoding a, Encoding b, Encoding c, Encoding d, Encoding e, Encoding f) => Encoding (a,b,c,d,e,f) where
  type Encoded (a,b,c,d,e,f) = (Encoded a, Encoded b, Encoded c, Encoded d, Encoded e, Encoded f)
  encode (a,b,c,d,e,f) = (encode a, encode b, encode c, encode d, encode e, encode f)

instance (Encoding a, Encoding b, Encoding c, Encoding d, Encoding e, Encoding f, Encoding g) => Encoding (a,b,c,d,e,f,g) where
  type Encoded (a,b,c,d,e,f,g) = (Encoded a, Encoded b, Encoded c, Encoded d, Encoded e, Encoded f, Encoded g)
  encode (a,b,c,d,e,f,g) = (encode a, encode b, encode c, encode d, encode e, encode f, encode g)

instance (Encoding a, Encoding b, Encoding c, Encoding d, Encoding e, Encoding f, Encoding g, Encoding h) => Encoding (a,b,c,d,e,f,g,h) where
  type Encoded (a,b,c,d,e,f,g,h) = (Encoded a, Encoded b, Encoded c, Encoded d, Encoded e, Encoded f, Encoded g, Encoded h)
  encode (a,b,c,d,e,f,g,h) = (encode a, encode b, encode c, encode d, encode e, encode f, encode g, encode h)

instance Encoding a => Encoding [a] where
  type Encoded [a] = [Encoded a]
  encode = encodeFunctor

instance (Ix i, Encoding e) => Encoding (Array i e) where
  type Encoded (Array i e) = Array i (Encoded e)
  encode = encodeFunctor

instance (Encoding a, Encoding b) => Encoding (Either a b) where
  type Encoded (Either a b) = Either (Encoded a) (Encoded b)
  encode (Left  a) = Left  (encode a)
  encode (Right b) = Right (encode b)

instance Encoding a => Encoding (HashMap k a) where
  type Encoded (HashMap k a) = HashMap k (Encoded a)
  encode = encodeFunctor

instance Encoding a => Encoding (IntMap a) where
  type Encoded (IntMap a) = IntMap (Encoded a)
  encode = encodeFunctor

instance Encoding a => Encoding (Map k a) where
  type Encoded (Map k a) = Map k (Encoded a)
  encode = encodeFunctor

instance Encoding a => Encoding (Maybe a) where
  type Encoded (Maybe a) = Maybe (Encoded a)
  encode = encodeFunctor

instance Encoding a => Encoding (Seq a) where
  type Encoded (Seq a) = Seq (Encoded a)
  encode = encodeFunctor

instance Encoding a => Encoding (Tree a) where
  type Encoded (Tree a) = Tree (Encoded a)
  encode = encodeFunctor

encodeFunctor :: (Functor f, Encoding a) => f (Encoded a) -> f a
encodeFunctor = fmap encode
