{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Safe #-}
--------------------------------------------------------------------
-- |
-- Copyright :  © Edward Kmett 2010-2013, Johan Kiviniemi 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ersatz.Decoding
  ( Decoding(..)
  ) where

import Control.Applicative
import Data.Array
import Data.HashMap.Lazy (HashMap)
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Traversable
import Data.Tree (Tree)
import Ersatz.Internal.Literal
import Ersatz.Solution

class Decoding a where
  type Decoded a :: *
  -- | Return a value based on the solution if one can be determined.
  decode :: Solution -> a -> Maybe (Decoded a)

instance Decoding Literal where
  type Decoded Literal = Bool
  decode = solutionLiteral

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
  decode = traverse . decode

instance (Ix i, Decoding e) => Decoding (Array i e) where
  type Decoded (Array i e) = Array i (Decoded e)
  decode = traverse . decode

instance (Decoding a, Decoding b) => Decoding (Either a b) where
  type Decoded (Either a b) = Either (Decoded a) (Decoded b)
  decode s (Left  a) = Left  <$> decode s a
  decode s (Right b) = Right <$> decode s b

instance Decoding a => Decoding (HashMap k a) where
  type Decoded (HashMap k a) = HashMap k (Decoded a)
  decode = traverse . decode

instance Decoding a => Decoding (IntMap a) where
  type Decoded (IntMap a) = IntMap (Decoded a)
  decode = traverse . decode

instance Decoding a => Decoding (Map k a) where
  type Decoded (Map k a) = Map k (Decoded a)
  decode = traverse . decode

instance Decoding a => Decoding (Maybe a) where
  type Decoded (Maybe a) = Maybe (Decoded a)
  decode = traverse . decode

instance Decoding a => Decoding (Seq a) where
  type Decoded (Seq a) = Seq (Decoded a)
  decode = traverse . decode

instance Decoding a => Decoding (Tree a) where
  type Decoded (Tree a) = Tree (Decoded a)
  decode = traverse . decode
