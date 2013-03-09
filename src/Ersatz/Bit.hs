{-# LANGUAGE TypeFamilies, TypeOperators, FlexibleInstances, DeriveDataTypeable, DeriveGeneric, DefaultSignatures, FlexibleContexts, UndecidableInstances #-}
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
module Ersatz.Bit
  ( Bit(..)
  , assert
  , Boolean(..)
  ) where

import Prelude hiding ((&&),(||),not,and,or)
import qualified Prelude

import Control.Applicative
import Data.Traversable (Traversable, traverse)
import Data.Typeable
import Ersatz.Decoding
import Ersatz.Encoding
import Ersatz.Monad
import Ersatz.Internal.Circuit
import Ersatz.Internal.Formula
import Ersatz.Internal.Literal
import Ersatz.Internal.StableName
import Ersatz.Solution
import Ersatz.Variable
import GHC.Generics
import System.IO.Unsafe

infixr 3 &&, &&#
infixr 2 ||, ||#
infixr 0 ==>, ==>#

-- Bit

-- | A 'Bit' provides a reference to a possibly indeterminate boolean
-- value that can be determined by an external SAT solver.
newtype Bit = Bit (Circuit Bit)
  deriving (Show, Typeable)

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
  x ==> y = not x || y
  not (Bit (Not c)) = c
  not (Bit (Var b)) = Bit (Var (negateLit b))
  not c        = Bit (Not c)
  a `xor` b    = Bit (Xor a b)
  and xs       = Bit (And xs)
  or xs        = Bit (Or xs)
  choose f t s = Bit (Mux f t s)

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

-- | Assert claims that in any satisf given 'Bit' must be 'true' in any 
-- satisfying interpretation of the current problem.
assert :: MonadSAT m => Bit -> m ()
assert b = do
  l <- runBit b
  assertFormula (formulaLiteral l)

-- | Convert a 'Bit' to a 'Literal'.
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

class GBoolean f where
  gbool :: Bool -> f a
  (&&#) :: f a -> f a -> f a
  (||#) :: f a -> f a -> f a
  (==>#) :: f a -> f a -> f a
  gnot :: f a -> f a
  gand :: [f a] -> f a
  gor  :: [f a] -> f a
  gxor :: f a -> f a -> f a

instance GBoolean U1 where
  gbool _    = U1
  U1 &&# U1  = U1
  U1 ||# U1  = U1
  U1 ==># U1 = U1
  gnot U1    = U1
  gand _     = U1
  gor  _     = U1
  gxor _ _   = U1

instance (GBoolean f, GBoolean g) => GBoolean (f :*: g) where
  gbool x = gbool x :*: gbool x
  (a :*: b) &&#  (c :*: d) = (a &&# c)  :*: (b &&# d)
  (a :*: b) ||#  (c :*: d) = (a ||# c)  :*: (b ||# d)
  (a :*: b) ==># (c :*: d) = (a ==># c) :*: (b ==># d)
  gnot (a :*: b) = gnot a :*: gnot b
  gand xs = gand (map (\(x :*: _) -> x) xs) :*: gand (map (\(_ :*: x) -> x) xs)
  gor xs = gor (map (\(x :*: _) -> x) xs) :*: gor (map (\(_ :*: x) -> x) xs)
  gxor (a :*: b) (c :*: d) = gxor a c :*: gxor b d

instance Boolean a => GBoolean (K1 i a) where
  gbool = K1 . bool
  K1 a &&# K1 b = K1 (a && b)
  K1 a ||# K1 b = K1 (a || b)
  K1 a ==># K1 b = K1 (a ==> b)
  gnot (K1 a) = K1 (not a)
  gand as = K1 (and (map (\(K1 a) -> a) as))
  gor as = K1 (or (map (\(K1 a) -> a) as))
  gxor (K1 a) (K1 b) = K1 (xor a b)

instance GBoolean a => GBoolean (M1 i c a) where
  gbool = M1 . gbool
  M1 a &&# M1 b = M1 (a &&# b)
  M1 a ||# M1 b = M1 (a ||# b)
  M1 a ==># M1 b = M1 (a ==># b)
  gnot (M1 a) = M1 (gnot a)
  gand as = M1 (gand (map (\(M1 a) -> a) as))
  gor as = M1 (gor (map (\(M1 a) -> a) as))
  gxor (M1 a) (M1 b) = M1 (gxor a b)

-- | The normal 'Bool' operators in Haskell are not overloaded. This
-- provides a richer set that are.
--
-- Instances for this class for product-like types can be automatically derived
-- for any type that is an instance of @Generic@
class Boolean t where
  -- | Lift a 'Bool'
  bool :: Bool -> t
  default bool :: (Generic t, GBoolean (Rep t)) => Bool -> t
  bool = to . gbool
  -- |
  -- @'true' = 'bool' 'True'@
  true :: t
  true = bool True
  -- |
  -- @'false' = 'bool' 'False'@
  false :: t
  false = bool False

  -- | Logical conjunction.
  (&&) :: t -> t -> t
  default (&&) :: (Generic t, GBoolean (Rep t)) => t -> t -> t
  x && y = to (from x &&# from y)

  -- | Logical disjunction (inclusive or).
  (||) :: t -> t -> t
  default (||) :: (Generic t, GBoolean (Rep t)) => t -> t -> t
  x || y = to (from x ||# from y)

  -- | Logical implication.
  (==>) :: t -> t -> t
  default (==>) :: (Generic t, GBoolean (Rep t)) => t -> t -> t
  x ==> y = to (from x ==># from y)

  -- | Logical negation
  not :: t -> t
  default not :: (Generic t, GBoolean (Rep t)) => t -> t
  not = to . gnot . from

  -- | The logical conjunction of several values.
  and :: [t] -> t
  default and :: (Generic t, GBoolean (Rep t)) => [t] -> t
  and = to . gand . map from

  -- | The logical disjunction of several values.
  or :: [t] -> t
  default or :: (Generic t, GBoolean (Rep t)) => [t] -> t
  or = to . gor . map from

  -- | The negated logical conjunction of several values.
  --
  -- @'nand' = 'not' . 'and'@
  nand :: [t] -> t
  nand = not . and

  -- | The negated logical disjunction of several values.
  --
  -- @'nor' = 'not' . 'or'@
  nor :: [t] -> t
  nor = not . or

  -- | Exclusive-or
  xor :: t -> t -> t
  default xor :: (Generic t, GBoolean (Rep t)) => t -> t -> t
  xor x y = to (from x `gxor` from y)

  -- | Choose between two alternatives based on a selector bit.
  choose :: t  -- ^ False branch
         -> t  -- ^ True branch
         -> t  -- ^ Predicate/selector branch
         -> t
  choose f t s = (f && not s) || (t && s)

instance Boolean Bool where
  bool = id
  true = True
  false = False

  (&&) = (Prelude.&&)
  (||) = (Prelude.||)
  x ==> y = not x || y

  not = Prelude.not

  and = Prelude.and
  or = Prelude.or

  False `xor` False = False
  False `xor` True  = True
  True `xor` False  = True
  True `xor` True   = False

  choose f _ False = f
  choose _ t True  = t
