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
  ( Encoding(..)
  , decodeTraversable
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

class Encoding a where
  type Decoded a :: *
  -- | Return a value based on the solution if one can be determined.
  decode :: Solution -> a -> IO (Maybe (Decoded a))

instance Encoding Literal where
  type Decoded Literal = Bool
  decode s l = return (solutionLiteral s l)

instance Encoding Lit where
  type Decoded Lit = Bool
  decode _ (Bool b) = return $ Just b
  decode s (Lit l)  = decode s l

instance Encoding () where
  type Decoded () = ()
  decode _ () = return $ Just ()

instance (Encoding a, Encoding b) => Encoding (a,b) where
  type Decoded (a,b) = (Decoded a, Decoded b)
  decode s (a,b) = liftA2 (,) <$> decode s a <*> decode s b

instance (Encoding a, Encoding b, Encoding c) => Encoding (a,b,c) where
  type Decoded (a,b,c) = (Decoded a, Decoded b, Decoded c)
  decode s (a,b,c) = liftA3 (,,) <$> decode s a <*> decode s b <*> decode s c

instance (Encoding a, Encoding b, Encoding c, Encoding d) => Encoding (a,b,c,d) where
  type Decoded (a,b,c,d) = (Decoded a, Decoded b, Decoded c, Decoded d)
  decode s (a,b,c,d) = go <$> decode s a <*> decode s b <*> decode s c <*> decode s d
    where go a' b' c' d' = (,,,) <$> a' <*> b' <*> c' <*> d'

instance (Encoding a, Encoding b, Encoding c, Encoding d, Encoding e) => Encoding (a,b,c,d,e) where
  type Decoded (a,b,c,d,e) = (Decoded a, Decoded b, Decoded c, Decoded d, Decoded e)
  decode s (a,b,c,d,e) = go <$> decode s a <*> decode s b <*> decode s c <*> decode s d <*> decode s e
    where go a' b' c' d' e' = (,,,,) <$> a' <*> b' <*> c' <*> d' <*> e'

instance (Encoding a, Encoding b, Encoding c, Encoding d, Encoding e, Encoding f) => Encoding (a,b,c,d,e,f) where
  type Decoded (a,b,c,d,e,f) = (Decoded a, Decoded b, Decoded c, Decoded d, Decoded e, Decoded f)
  decode s (a,b,c,d,e,f) = go <$> decode s a <*> decode s b <*> decode s c <*> decode s d <*> decode s e <*> decode s f
    where go a' b' c' d' e' f' = (,,,,,) <$> a' <*> b' <*> c' <*> d' <*> e' <*> f'

instance (Encoding a, Encoding b, Encoding c, Encoding d, Encoding e, Encoding f, Encoding g) => Encoding (a,b,c,d,e,f,g) where
  type Decoded (a,b,c,d,e,f,g) = (Decoded a, Decoded b, Decoded c, Decoded d, Decoded e, Decoded f, Decoded g)
  decode s (a,b,c,d,e,f,g) = go <$> decode s a <*> decode s b <*> decode s c <*> decode s d <*> decode s e <*> decode s f <*> decode s g
    where go a' b' c' d' e' f' g' = (,,,,,,) <$> a' <*> b' <*> c' <*> d' <*> e' <*> f' <*> g'

instance (Encoding a, Encoding b, Encoding c, Encoding d, Encoding e, Encoding f, Encoding g, Encoding h) => Encoding (a,b,c,d,e,f,g,h) where
  type Decoded (a,b,c,d,e,f,g,h) = (Decoded a, Decoded b, Decoded c, Decoded d, Decoded e, Decoded f, Decoded g, Decoded h)
  decode s (a,b,c,d,e,f,g,h) = go <$> decode s a <*> decode s b <*> decode s c <*> decode s d <*> decode s e <*> decode s f <*> decode s g <*> decode s h
    where go a' b' c' d' e' f' g' h' = (,,,,,,,) <$> a' <*> b' <*> c' <*> d' <*> e' <*> f' <*> g' <*> h'

instance Encoding a => Encoding [a] where
  type Decoded [a] = [Decoded a]
  decode = decodeTraversable

instance (Ix i, Encoding e) => Encoding (Array i e) where
  type Decoded (Array i e) = Array i (Decoded e)
  decode = decodeTraversable

instance (Encoding a, Encoding b) => Encoding (Either a b) where
  type Decoded (Either a b) = Either (Decoded a) (Decoded b)
  decode s (Left  a) = fmap Left  <$> decode s a
  decode s (Right b) = fmap Right <$> decode s b

instance Encoding a => Encoding (HashMap k a) where
  type Decoded (HashMap k a) = HashMap k (Decoded a)
  decode = decodeTraversable

instance Encoding a => Encoding (IntMap a) where
  type Decoded (IntMap a) = IntMap (Decoded a)
  decode = decodeTraversable

instance Encoding a => Encoding (Map k a) where
  type Decoded (Map k a) = Map k (Decoded a)
  decode = decodeTraversable

instance Encoding a => Encoding (Maybe a) where
  type Decoded (Maybe a) = Maybe (Decoded a)
  decode = decodeTraversable

instance Encoding a => Encoding (Seq a) where
  type Decoded (Seq a) = Seq (Decoded a)
  decode = decodeTraversable

instance Encoding a => Encoding (Tree a) where
  type Decoded (Tree a) = Tree (Decoded a)
  decode = decodeTraversable

decodeTraversable :: (Traversable f, Encoding a) => Solution -> f a -> IO (Maybe (f (Decoded a)))
decodeTraversable s a = sequenceA <$> traverse (decode s) a
