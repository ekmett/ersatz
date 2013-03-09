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
module Ersatz.Encoding
  ( Encoding(..)
  ) where

import Data.Array
import Data.HashMap.Lazy (HashMap)
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Tree (Tree)
import Ersatz.Internal.Literal

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
  encode = fmap encode

instance (Ix i, Encoding e) => Encoding (Array i e) where
  type Encoded (Array i e) = Array i (Encoded e)
  encode = fmap encode

instance (Encoding a, Encoding b) => Encoding (Either a b) where
  type Encoded (Either a b) = Either (Encoded a) (Encoded b)
  encode (Left  a) = Left  (encode a)
  encode (Right b) = Right (encode b)

instance Encoding a => Encoding (HashMap k a) where
  type Encoded (HashMap k a) = HashMap k (Encoded a)
  encode = fmap encode

instance Encoding a => Encoding (IntMap a) where
  type Encoded (IntMap a) = IntMap (Encoded a)
  encode = fmap encode

instance Encoding a => Encoding (Map k a) where
  type Encoded (Map k a) = Map k (Encoded a)
  encode = fmap encode

instance Encoding a => Encoding (Maybe a) where
  type Encoded (Maybe a) = Maybe (Encoded a)
  encode = fmap encode

instance Encoding a => Encoding (Seq a) where
  type Encoded (Seq a) = Seq (Encoded a)
  encode = fmap encode

instance Encoding a => Encoding (Tree a) where
  type Encoded (Tree a) = Tree (Encoded a)
  encode = fmap encode
