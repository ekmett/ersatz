{-# LANGUAGE TypeFamilies, TypeOperators, FlexibleInstances #-}
module Data.Logic.Ersatz.Internal.Bit
    ( Boolean(..)
    , Equatable(..)
    , Bit(..)
    , bit
    , Circuit(..)
--  , assertBit
    ) where

import Prelude hiding ((&&),(||),not,and,or)
import qualified Prelude
import Control.Applicative
import Data.List (partition)
import Data.Traversable (Traversable,traverse)
import Data.Logic.Ersatz.Encoding (Encoding(..))
import Data.Logic.Ersatz.Internal.Problem
import Data.Reify
-- import Data.Reify.Graph -- Logic.Ersatz.Internal.Reify

infix  4 ===, /==
infixr 3 &&
infixr 2 ||
infixr 0 ==>

class Boolean t where
    bool :: Bool -> t
    true :: t
    false :: t
    (&&) :: t -> t -> t
    (||) :: t -> t -> t
    (==>) :: t -> t -> t
    not :: t -> t
    and :: [t] -> t
    or :: [t] -> t
    nand :: [t] -> t
    nor :: [t] -> t
    xor :: t -> t -> t

    -- map a 2x2 multiplexor over the functor
    choose :: Functor f => t -> f (t,t) -> f t
    choose s = fmap (\(a,b) -> (a && not s) || (b && s))

    x ==> y = not x || y
    nand = not . and
    nor = not . or

bit :: Bool -> Bit
bit = bool

instance Boolean Bool where
    bool = id
    true = True
    false = False

    (&&) = (Prelude.&&)
    (||) = (Prelude.||)

    not = Prelude.not

    and = Prelude.and
    or = Prelude.or

    False `xor` False = False
    False `xor` True  = True
    True `xor` False  = True
    True `xor` True   = False

    choose False = fmap fst
    choose True = fmap snd

data Circuit c
    = And [c]
    | Or [c]
    | Xor c c
    | Not c
    | Var !Lit

-- does this have to be data?
newtype Bit = Bit (Circuit Bit)

-- a Bit you don't assert is actually a boolean function that you can evaluate later after compilation
instance Encoding Bit where
    type Decoded Bit = Bool
    decode f (Bit c) = case c of
        And cs  -> all (decode f) cs
        Or cs   -> any (decode f) cs
        Xor x y -> xor (decode f x) (decode f y)
        Not c'   -> not (decode f c')
        Var l   -> decode f l

    decide f (Bit c) = case c of
        And cs -> let (knowns, unknowns) = partition (decide f) cs
                  in not (all (decode f) knowns) || null unknowns
        Or cs  -> let (knowns, unknowns) = partition (decide f) cs
                  in any (decode f) knowns || null unknowns
        Xor x y -> decide f x && decide f y
        Not c'  -> decide f c'
        Var l   -> decide f l

instance Boolean Bit where
    -- improve the stablemap this way
    bool t | t = true
           | otherwise = false
    true  = Bit (Var (lit True))
    false = Bit (Var (lit False))
    a && b = Bit (And [a,b])
    a || b = Bit (Or [a,b])
    not (Bit (Not c)) = c
    not (Bit (Var b)) = Bit (Var (negateLit b))
    not c     = Bit (Not c)
    a `xor` b = Bit (Xor a b)
    and xs  = Bit (And xs)
    or xs   = Bit (Or xs)

class Equatable t where
    (===) :: t -> t -> Bit
    (/==) :: t -> t -> Bit

    a === b = not (a /== b)
    a /== b = not (a === b)

instance Equatable Bit where
    (/==) = xor
instance (Equatable a, Equatable b) => Equatable (a,b) where
    (a,b) === (a',b') = a === a' && b === b'
instance (Equatable a, Equatable b, Equatable c) => Equatable (a,b,c) where
    (a,b,c) === (a',b',c') = a === a' && b === b' && c === c'
instance (Equatable a, Equatable b, Equatable c, Equatable d) => Equatable (a,b,c,d) where
    (a,b,c,d) === (a',b',c',d') = a === a' && b === b' && c === c' && d === d'
instance (Equatable a, Equatable b, Equatable c, Equatable d, Equatable e) => Equatable (a,b,c,d,e) where
    (a,b,c,d,e) === (a',b',c',d',e') = a === a' && b === b' && c === c' && d === d' && e === e'

instance Equatable [Bit] where
    as === bs = bool (length as == length bs) && and (zipWith (===) as bs)

instance Variable Bit where
    known (Bit b) = case b of
        And xs ->  all known xs
        Or xs -> any known xs
        Not c -> known c
        Xor c d -> known c && known d
        Var n -> known n
    exists = Bit . Var <$> exists
    forall = Bit . Var <$> forall

instance MuRef Bit where
    type DeRef Bit = Circuit
    mapDeRef f (Bit b) = case b of
        And xs -> And <$> traverse f xs
        Or xs -> Or <$> traverse f xs
        Not x -> Not <$> f x
        Xor x y -> Xor <$> f x <*> f y
        Var n -> pure $ Var n

{-
data Graph e b = Graph [(Plan b, e Plan b)] (Plan b)

reifyLit :: (MonadSAT s m, MuRef f) => Bit s -> m ((Lit s, [ ] )
reifyLit a = a `seq` do
        k <- liftST $ makeDynStableName root
        l <- lookupDyn k
        case l of
            Nothing -> do
                v <- exists -- Lit
                insertDyn k v
                mapDeRef reifyLit a
                return v
            Just v -> return v
-}
