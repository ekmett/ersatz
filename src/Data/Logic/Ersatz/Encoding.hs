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
  ( Decode(..)
  , decodeTraversable
  , Encode(..)
  , encodeFunctor
  ) where

import Control.Applicative
import Data.Array
import Data.HashMap.Lazy (HashMap)
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Traversable (Traversable, sequenceA, traverse)
import Data.Tree (Tree)

import Data.Logic.Ersatz.Internal.Problem
import Data.Logic.Ersatz.Solution

class Decode a where
  type Decoded a :: *
  -- | Return a value based on the solution if one can be determined.
  decode :: Solution -> a -> IO (Maybe (Decoded a))

instance Decode Literal where
  type Decoded Literal = Bool
  decode s l = return (solutionLiteral s l)

instance Decode Lit where
  type Decoded Lit = Bool
  decode _ (Bool b) = return $ Just b
  decode s (Lit l)  = decode s l

instance Decode () where
  type Decoded () = ()
  decode _ () = return $ Just ()

instance (Decode a, Decode b) => Decode (a,b) where
  type Decoded (a,b) = (Decoded a, Decoded b)
  decode s (a,b) = liftA2 (,) <$> decode s a <*> decode s b

instance (Decode a, Decode b, Decode c) => Decode (a,b,c) where
  type Decoded (a,b,c) = (Decoded a, Decoded b, Decoded c)
  decode s (a,b,c) = liftA3 (,,) <$> decode s a <*> decode s b <*> decode s c

instance (Decode a, Decode b, Decode c, Decode d) => Decode (a,b,c,d) where
  type Decoded (a,b,c,d) = (Decoded a, Decoded b, Decoded c, Decoded d)
  decode s (a,b,c,d) = go <$> decode s a <*> decode s b <*> decode s c <*> decode s d
    where go a' b' c' d' = (,,,) <$> a' <*> b' <*> c' <*> d'

instance (Decode a, Decode b, Decode c, Decode d, Decode e) => Decode (a,b,c,d,e) where
  type Decoded (a,b,c,d,e) = (Decoded a, Decoded b, Decoded c, Decoded d, Decoded e)
  decode s (a,b,c,d,e) = go <$> decode s a <*> decode s b <*> decode s c <*> decode s d <*> decode s e
    where go a' b' c' d' e' = (,,,,) <$> a' <*> b' <*> c' <*> d' <*> e'

instance (Decode a, Decode b, Decode c, Decode d, Decode e, Decode f) => Decode (a,b,c,d,e,f) where
  type Decoded (a,b,c,d,e,f) = (Decoded a, Decoded b, Decoded c, Decoded d, Decoded e, Decoded f)
  decode s (a,b,c,d,e,f) = go <$> decode s a <*> decode s b <*> decode s c <*> decode s d <*> decode s e <*> decode s f
    where go a' b' c' d' e' f' = (,,,,,) <$> a' <*> b' <*> c' <*> d' <*> e' <*> f'

instance (Decode a, Decode b, Decode c, Decode d, Decode e, Decode f, Decode g) => Decode (a,b,c,d,e,f,g) where
  type Decoded (a,b,c,d,e,f,g) = (Decoded a, Decoded b, Decoded c, Decoded d, Decoded e, Decoded f, Decoded g)
  decode s (a,b,c,d,e,f,g) = go <$> decode s a <*> decode s b <*> decode s c <*> decode s d <*> decode s e <*> decode s f <*> decode s g
    where go a' b' c' d' e' f' g' = (,,,,,,) <$> a' <*> b' <*> c' <*> d' <*> e' <*> f' <*> g'

instance (Decode a, Decode b, Decode c, Decode d, Decode e, Decode f, Decode g, Decode h) => Decode (a,b,c,d,e,f,g,h) where
  type Decoded (a,b,c,d,e,f,g,h) = (Decoded a, Decoded b, Decoded c, Decoded d, Decoded e, Decoded f, Decoded g, Decoded h)
  decode s (a,b,c,d,e,f,g,h) = go <$> decode s a <*> decode s b <*> decode s c <*> decode s d <*> decode s e <*> decode s f <*> decode s g <*> decode s h
    where go a' b' c' d' e' f' g' h' = (,,,,,,,) <$> a' <*> b' <*> c' <*> d' <*> e' <*> f' <*> g' <*> h'

instance Decode a => Decode [a] where
  type Decoded [a] = [Decoded a]
  decode = decodeTraversable

instance (Ix i, Decode e) => Decode (Array i e) where
  type Decoded (Array i e) = Array i (Decoded e)
  decode = decodeTraversable

instance (Decode a, Decode b) => Decode (Either a b) where
  type Decoded (Either a b) = Either (Decoded a) (Decoded b)
  decode s (Left  a) = fmap Left  <$> decode s a
  decode s (Right b) = fmap Right <$> decode s b

instance Decode a => Decode (HashMap k a) where
  type Decoded (HashMap k a) = HashMap k (Decoded a)
  decode = decodeTraversable

instance Decode a => Decode (IntMap a) where
  type Decoded (IntMap a) = IntMap (Decoded a)
  decode = decodeTraversable

instance Decode a => Decode (Map k a) where
  type Decoded (Map k a) = Map k (Decoded a)
  decode = decodeTraversable

instance Decode a => Decode (Maybe a) where
  type Decoded (Maybe a) = Maybe (Decoded a)
  decode = decodeTraversable

instance Decode a => Decode (Seq a) where
  type Decoded (Seq a) = Seq (Decoded a)
  decode = decodeTraversable

instance Decode a => Decode (Tree a) where
  type Decoded (Tree a) = Tree (Decoded a)
  decode = decodeTraversable

decodeTraversable :: (Traversable f, Decode a) => Solution -> f a -> IO (Maybe (f (Decoded a)))
decodeTraversable s a = sequenceA <$> traverse (decode s) a

class Encode a where
  type Encoded a :: *
  encode :: Encoded a -> a

instance Encode Lit where
  type Encoded Lit = Bool
  encode = Bool

instance Encode () where
  type Encoded () = ()
  encode () = ()

instance (Encode a, Encode b) => Encode (a,b) where
  type Encoded (a,b) = (Encoded a, Encoded b)
  encode (a,b) = (encode a, encode b)

instance (Encode a, Encode b, Encode c) => Encode (a,b,c) where
  type Encoded (a,b,c) = (Encoded a, Encoded b, Encoded c)
  encode (a,b,c) = (encode a, encode b, encode c)

instance (Encode a, Encode b, Encode c, Encode d) => Encode (a,b,c,d) where
  type Encoded (a,b,c,d) = (Encoded a, Encoded b, Encoded c, Encoded d)
  encode (a,b,c,d) = (encode a, encode b, encode c, encode d)

instance (Encode a, Encode b, Encode c, Encode d, Encode e) => Encode (a,b,c,d,e) where
  type Encoded (a,b,c,d,e) = (Encoded a, Encoded b, Encoded c, Encoded d, Encoded e)
  encode (a,b,c,d,e) = (encode a, encode b, encode c, encode d, encode e)

instance (Encode a, Encode b, Encode c, Encode d, Encode e, Encode f) => Encode (a,b,c,d,e,f) where
  type Encoded (a,b,c,d,e,f) = (Encoded a, Encoded b, Encoded c, Encoded d, Encoded e, Encoded f)
  encode (a,b,c,d,e,f) = (encode a, encode b, encode c, encode d, encode e, encode f)

instance (Encode a, Encode b, Encode c, Encode d, Encode e, Encode f, Encode g) => Encode (a,b,c,d,e,f,g) where
  type Encoded (a,b,c,d,e,f,g) = (Encoded a, Encoded b, Encoded c, Encoded d, Encoded e, Encoded f, Encoded g)
  encode (a,b,c,d,e,f,g) = (encode a, encode b, encode c, encode d, encode e, encode f, encode g)

instance (Encode a, Encode b, Encode c, Encode d, Encode e, Encode f, Encode g, Encode h) => Encode (a,b,c,d,e,f,g,h) where
  type Encoded (a,b,c,d,e,f,g,h) = (Encoded a, Encoded b, Encoded c, Encoded d, Encoded e, Encoded f, Encoded g, Encoded h)
  encode (a,b,c,d,e,f,g,h) = (encode a, encode b, encode c, encode d, encode e, encode f, encode g, encode h)

instance Encode a => Encode [a] where
  type Encoded [a] = [Encoded a]
  encode = encodeFunctor

instance (Ix i, Encode e) => Encode (Array i e) where
  type Encoded (Array i e) = Array i (Encoded e)
  encode = encodeFunctor

instance (Encode a, Encode b) => Encode (Either a b) where
  type Encoded (Either a b) = Either (Encoded a) (Encoded b)
  encode (Left  a) = Left  (encode a)
  encode (Right b) = Right (encode b)

instance Encode a => Encode (HashMap k a) where
  type Encoded (HashMap k a) = HashMap k (Encoded a)
  encode = encodeFunctor

instance Encode a => Encode (IntMap a) where
  type Encoded (IntMap a) = IntMap (Encoded a)
  encode = encodeFunctor

instance Encode a => Encode (Map k a) where
  type Encoded (Map k a) = Map k (Encoded a)
  encode = encodeFunctor

instance Encode a => Encode (Maybe a) where
  type Encoded (Maybe a) = Maybe (Encoded a)
  encode = encodeFunctor

instance Encode a => Encode (Seq a) where
  type Encoded (Seq a) = Seq (Encoded a)
  encode = encodeFunctor

instance Encode a => Encode (Tree a) where
  type Encoded (Tree a) = Tree (Encoded a)
  encode = encodeFunctor

encodeFunctor :: (Functor f, Encode a) => f (Encoded a) -> f a
encodeFunctor = fmap encode
