module Logic.QSAT.Encoding 
    ( Encoding(..)
    , decodeFunctor
    ) where

class Encoding t where
    type Decoded t :: *
    decode :: (Literal b -> Bool) -> t b -> Decoded t

{-
-- decode :: (Literal b -> Bool) -> t b -> Decoded t
-- decodeFunctor :: Functor f => (Literal b -> Bool) -> f (t b) -> f (Decoded t)
    
decodeFunctor :: Functor f => (Literal (Brand t) -> Bool) -> f t -> f (Decoded t)
decodeFunctor f = fmap (decode f)

instance Encoding a => Encoding [a] where
    type Decoded [a] = [Decoded a]
    type Brand [a] = Brand a
    decode = decodeFunctor

instance Encoding a => Encoding (Seq a) where
    type Decoded (Seq a) = Seq (Decoded a) 
    type Brand (Seq a) = Brand a
    decode = decodeFunctor

instance Encoding a => Encoding (Maybe a) where
    type Decoded (Maybe a) = Maybe (Decoded a)
    type Brand (Maybe a) = Brand a 
    decode = decodeFunctor

instance (Ix i, Encoding t) => Encoding (Array i t) where
    type Decoded (Array i t) = Array i (Decoded t)
    type Brand (Array i t) = Brand t
    decode = decodeFunctor

instance (Brand a ~ Brand b, Encoding a, Encoding b) => Encoding (Either a b) where
    type Decoded (Either a b) = Either (Decoded a) (Decode b)
    type Brand (Either a b) = Brand a
    decode (Left a) = Left (decode a)
    decode (Right b) = Right (decode b)

instance (Brand a ~ Brand b, Encoding a, Encoding b) => Encoding (a,b) where
    type Decoded (a,b) = (Decoded a, Decoded b)
    decode f (a,b) = (decode f a, decode f b)

instance (Brand a ~ Brand b, Brand a ~ Brand c, Encoding a, Encoding b, Encoding c) => Encoding (a, b, c) where
    type Decoded (a,b,c) = (Decoded a, Decoded b, Decoded c)
    decode f (a,b,c) = (decode f a, decode f b, decode f c)

instance (Brand a ~ Brand b, Brand a ~ Brand c, Brand a ~ Brand d, Encoding a, Encoding b, Encoding c, Encoding d) => Encoding (a, b, c, d) where
    type Decoded (a,b,c,d) = (Decoded a, Decoded b, Decoded c, Decoded d) 
    decode f (a,b,c,d) = (decode f a, decode f b, decode f c, decode f d)

instance (Brand a ~ Brand b, Brand a ~ Brand c, Brand a ~ Brand d, Brand a ~ Brand e, Encoding a, Encoding b, Encoding c, Encoding d, Encoding e) => Encoding (a, b, c, d, e) where
    type Decoded (a,b,c,d) = (Decoded a, Decoded b, Decoded c, Decoded d, Decoded e) 
    decode f (a,b,c,d) = (decode f a, decode f b, decode f c, decode f d, decode f e)

-}
