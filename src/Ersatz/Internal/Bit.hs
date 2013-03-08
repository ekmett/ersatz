{-# LANGUAGE TypeFamilies, TypeOperators, FlexibleInstances, DeriveDataTypeable #-}
{-# OPTIONS_HADDOCK not-home #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2010-2013, Johan Kiviniemi 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ersatz.Internal.Bit
  ( Bit(..)
  , Circuit(..)
  , assert
  , Boolean(..)
  , Equatable(..)
  ) where

import Prelude hiding ((&&),(||),not,and,or)
import qualified Prelude

import Control.Applicative
import Data.Foldable (Foldable, foldMap)
import Data.Monoid
import Data.Traversable (Traversable, traverse)
import Data.Typeable
import Ersatz.Decoding
import Ersatz.Encoding
import Ersatz.Internal.Problem
import Ersatz.Internal.StableName
import Ersatz.Solution
import System.IO.Unsafe

infix  4 ===, /==
infixr 3 &&
infixr 2 ||
infixr 0 ==>

-- Bit

newtype Bit = Bit (Circuit Bit)
  deriving (Show, Typeable)

data Circuit c
  = And [c]
  | Or [c]
  | Xor c c
  | Mux c c c  -- ^ False branch, true branch, predicate/selector branch
  | Not c
  | Var !Lit
  deriving (Show, Typeable)

instance Functor Circuit where
  fmap f (And as) = And (map f as)
  fmap f (Or as) = Or (map f as)
  fmap f (Xor a b) = Xor (f a) (f b)
  fmap f (Mux a b c) = Mux (f a) (f b) (f c)
  fmap f (Not a) = Not (f a)
  fmap _ (Var l) = Var l

instance Foldable Circuit where
  foldMap f (And as) = foldMap f as
  foldMap f (Or as) = foldMap f as
  foldMap f (Xor a b) = f a <> f b
  foldMap f (Mux a b c) = f a <> f b <> f c
  foldMap f (Not a) = f a
  foldMap _ Var{} = mempty

instance Traversable Circuit where
  traverse f (And as) = And <$> traverse f as
  traverse f (Or as) = Or <$> traverse f as
  traverse f (Xor a b) = Xor <$> f a <*> f b
  traverse f (Mux a b c) = Mux <$> f a <*> f b <*> f c
  traverse f (Not a) = Not <$> f a
  traverse _ (Var l) = pure (Var l)

instance Boolean Bit where
  -- improve the stablemap this way
  bool True  = true
  bool False = false
  true  = Bit (Var (lit True))
  false = Bit (Var (lit False))
  Bit (And as) && Bit (And bs) = and (as ++ bs)
  Bit (And as) && b            = and (as ++ [b])
  a            && Bit (And bs) = and (a : bs)
  a            && b            = and [a,b]
  Bit (Or as) || Bit (Or bs) = or (as ++ bs)
  Bit (Or as) || b           = or (as ++ [b])
  a           || Bit (Or bs) = or (a : bs)
  a           || b           = or [a,b]
  not (Bit (Not c)) = c
  not (Bit (Var b)) = Bit (Var (negateLit b))
  not c     = Bit (Not c)
  a `xor` b = Bit (Xor a b)
  and xs  = Bit (And xs)
  or xs   = Bit (Or xs)
  choose f t s = Bit (Mux f t s)

instance Equatable Bit where
  (/==) = xor

instance Variable Bit where
  exists = Bit . Var <$> exists
  forall = Bit . Var <$> forall

-- a Bit you don't assert is actually a boolean function that you can evaluate later after compilation
instance Decoding Bit where
  type Decoded Bit = Bool
  decode sol b@(Bit c) 
      = solutionStableName sol (unsafePerformIO (makeStableName' b))
     -- The StableName didn’t have an associated literal with a solution,
     -- recurse to compute the value.
    <|> case c of
          And cs  -> andMaybeBools $ decode sol <$> cs
          Or cs   -> orMaybeBools  $ decode sol <$> cs
          Xor x y -> xor <$> decode sol x <*> decode sol y
          Mux cf ct cp -> do
            p <- decode sol cp
            decode sol $ if p then ct else cf
          Not c'  -> not <$> decode sol c'
          Var l   -> decode sol l
    where
      andMaybeBools :: [Maybe Bool] -> Maybe Bool
      andMaybeBools mbs
        | any not knowns = Just False  -- One is known to be false.
        | null unknowns  = Just True   -- All are known to be true.
        | otherwise      = Nothing     -- Unknown.
        where
          (unknowns, knowns) = partitionMaybes mbs

      orMaybeBools :: [Maybe Bool] -> Maybe Bool
      orMaybeBools mbs
        | or knowns     = Just True   -- One is known to be true.
        | null unknowns = Just False  -- All are known to be false.
        | otherwise     = Nothing     -- Unknown.
        where
          (unknowns, knowns) = partitionMaybes mbs

      partitionMaybes :: [Maybe a] -> ([()], [a])
      partitionMaybes = foldr go ([],[])
        where
          go Nothing  ~(ns, js) = (():ns, js)
          go (Just a) ~(ns, js) = (ns,    a:js)

instance Encoding Bit where
  type Encoded Bit = Bool
  encode = bool

assert :: MonadSAT m => Bit -> m ()
assert b = do
  l <- runBit b
  assertFormula (formulaLiteral l)

runBit :: MonadSAT m => Bit -> m Literal
runBit (Bit (Not c)) = negateLiteral <$> runBit c
runBit (Bit (Var (Lit l))) = return l
runBit b@(Bit c) = generateLiteral b $ \out ->
  assertFormula =<< case c of
    And bs    -> formulaAnd out <$> traverse runBit bs
    Or  bs    -> formulaOr  out <$> traverse runBit bs
    Xor x y   -> formulaXor out <$> runBit x <*> runBit y
    Mux x y p -> formulaMux out <$> runBit x <*> runBit y <*> runBit p
    Var (Bool False) -> return $ formulaLiteral (negateLiteral out)
    Var (Bool True)  -> return $ formulaLiteral out

    -- Already handled above but GHC doesn't realize it.
    Not _       -> error "Unreachable"
    Var (Lit _) -> error "Unreachable"

-- Boolean

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

  choose :: t  -- ^ False branch
         -> t  -- ^ True branch
         -> t  -- ^ Predicate/selector branch
         -> t
  choose f t s = (f && not s) || (t && s)

  x ==> y = not x || y
  nand = not . and
  nor = not . or

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

  choose f _ False = f
  choose _ t True  = t

-- Equatable

class Equatable t where
  (===) :: t -> t -> Bit
  (/==) :: t -> t -> Bit

  a === b = not (a /== b)
  a /== b = not (a === b)

instance (Equatable a, Equatable b) => Equatable (a,b) where
  (a,b) === (a',b') = a === a' && b === b'
instance (Equatable a, Equatable b, Equatable c) => Equatable (a,b,c) where
  (a,b,c) === (a',b',c') = a === a' && b === b' && c === c'
instance (Equatable a, Equatable b, Equatable c, Equatable d) => Equatable (a,b,c,d) where
  (a,b,c,d) === (a',b',c',d') = a === a' && b === b' && c === c' && d === d'
instance (Equatable a, Equatable b, Equatable c, Equatable d, Equatable e) => Equatable (a,b,c,d,e) where
  (a,b,c,d,e) === (a',b',c',d',e') = a === a' && b === b' && c === c' && d === d' && e === e'
instance (Equatable a, Equatable b, Equatable c, Equatable d, Equatable e, Equatable f) => Equatable (a,b,c,d,e,f) where
  (a,b,c,d,e,f) === (a',b',c',d',e',f') = a === a' && b === b' && c === c' && d === d' && e === e' && f === f'
instance (Equatable a, Equatable b, Equatable c, Equatable d, Equatable e, Equatable f, Equatable g) => Equatable (a,b,c,d,e,f,g) where
  (a,b,c,d,e,f,g) === (a',b',c',d',e',f',g') = a === a' && b === b' && c === c' && d === d' && e === e' && f === f' && g === g'
instance (Equatable a, Equatable b, Equatable c, Equatable d, Equatable e, Equatable f, Equatable g, Equatable h) => Equatable (a,b,c,d,e,f,g,h) where
  (a,b,c,d,e,f,g,h) === (a',b',c',d',e',f',g',h') = a === a' && b === b' && c === c' && d === d' && e === e' && f === f' && g === g' && h === h'

instance Equatable a => Equatable [a] where
  as === bs | length as /= length bs = false
            | otherwise              = and (zipWith (===) as bs)
