{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UndecidableInstances #-}

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

import Prelude hiding ((&&),(||),not,and,or,all,any)
import qualified Prelude

import Control.Applicative
import Control.Monad.State
import Data.Foldable (Foldable, toList)
import qualified Data.Foldable as Foldable
import Data.Sequence ((<|), (|>), (><))
import qualified Data.Sequence as Seq
import Data.Typeable
import Ersatz.Decoding
import Ersatz.Encoding
import Ersatz.Internal.Circuit
import Ersatz.Internal.Formula
import Ersatz.Internal.Literal
import Ersatz.Internal.StableName
import Ersatz.Problem
import Ersatz.Solution
import Ersatz.Variable
import GHC.Generics
import System.IO.Unsafe

infixr 3 &&, &&#
infixr 2 ||, ||#
infixr 0 ==>

-- Bit

-- | A 'Bit' provides a reference to a possibly indeterminate boolean
-- value that can be determined by an external SAT solver.
newtype Bit = Bit (Circuit Bit)
  deriving (Show, Typeable)

instance Boolean Bit where
  -- improve the stablemap this way
  bool True  = true
  bool False = false
  true  = Bit (Var literalTrue)
  false = Bit (Var literalFalse)

  a@(Bit (Var (Literal (-1)))) && _ = a
  _ && b@(Bit (Var (Literal (-1)))) = b
  a && Bit (Var (Literal 1)) = a
  Bit (Var (Literal 1)) && b = b
  Bit (And as) && Bit (And bs) = Bit (And (as >< bs))
  Bit (And as) && b            = Bit (And (as |> b))
  a            && Bit (And bs) = Bit (And (a <| bs))
  a            && b            = Bit (And (a <| b <| Seq.empty))

  a || Bit (Var (Literal (-1))) = a
  Bit (Var (Literal (-1))) || b = b
  a@(Bit (Var (Literal 1))) || _ = a
  _ || b@(Bit (Var (Literal 1))) = b
  Bit (Or as) || Bit (Or bs) = Bit (Or (as >< bs))
  Bit (Or as) || b           = Bit (Or (as |> b))
  a           || Bit (Or bs) = Bit (Or (a <| bs))
  a           || b           = Bit (Or (a <| b <| Seq.empty))

  not (Bit (Not c)) = c
  not (Bit (Var l)) = Bit (Var (negateLiteral l))
  not c        = Bit (Not c)

  a `xor` Bit (Var (Literal (-1))) = a
  Bit (Var (Literal (-1))) `xor` b = b
  a `xor` Bit (Var (Literal 1))    = not a
  Bit (Var (Literal 1))    `xor` b = not b
  a `xor` b    = Bit (Xor a b)

  and = Foldable.foldl' (&&) true
  or  = Foldable.foldl' (||) false

  all p = Foldable.foldl' (\res b -> res && p b) true
  any p = Foldable.foldl' (\res b -> res || p b) false

  choose f _ (Bit (Var (Literal (-1)))) = f
  choose _ t (Bit (Var (Literal 1)))    = t
  choose f t s = Bit (Mux f t s)

instance Variable Bit where
  exists = liftM (Bit . Var) exists
  forall = liftM (Bit . Var) forall

-- a Bit you don't assert is actually a boolean function that you can evaluate later after compilation
instance Decoding Bit where
  type Decoded Bit = Bool
  decode sol b@(Bit c) 
      = solutionStableName sol (unsafePerformIO (makeStableName' b))
     -- The StableName didn’t have an associated literal with a solution,
     -- recurse to compute the value.
    <|> case c of
          And cs  -> andMaybeBools . toList $ decode sol <$> cs
          Or cs   -> orMaybeBools  . toList $ decode sol <$> cs
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

-- | Assert claims that 'Bit' must be 'true' in any satisfying interpretation
-- of the current problem.
assert :: (MonadState s m, HasSAT s) => Bit -> m ()
assert b = do
  l <- runBit b
  assertFormula (formulaLiteral l)

-- | Convert a 'Bit' to a 'Literal'.
runBit :: (MonadState s m, HasSAT s) => Bit -> m Literal
runBit (Bit (Not c)) = negateLiteral `liftM` runBit c
runBit (Bit (Var l)) = return l
runBit b@(Bit c) = generateLiteral b $ \out ->
  assertFormula =<< case c of
    And bs    -> formulaAnd out `liftM` mapM runBit (toList bs)
    Or  bs    -> formulaOr  out `liftM` mapM runBit (toList bs)
    Xor x y   -> liftM2 (formulaXor out) (runBit x) (runBit y)
    Mux x y p -> liftM3 (formulaMux out) (runBit x) (runBit y) (runBit p)

    -- Already handled above but GHC doesn't realize it.
    Not _ -> error "Unreachable"
    Var _ -> error "Unreachable"

class GBoolean f where
  gbool :: Bool -> f a
  (&&#) :: f a -> f a -> f a
  (||#) :: f a -> f a -> f a
  gnot :: f a -> f a
  gall :: Foldable t => (a -> f b) -> t a -> f b
  gany :: Foldable t => (a -> f b) -> t a -> f b
  gxor :: f a -> f a -> f a

instance GBoolean U1 where
  gbool _    = U1
  U1 &&# U1  = U1
  U1 ||# U1  = U1
  gnot U1    = U1
  gall _ _   = U1
  gany _ _   = U1
  gxor _ _   = U1

instance (GBoolean f, GBoolean g) => GBoolean (f :*: g) where
  gbool x = gbool x :*: gbool x
  (a :*: b) &&#  (c :*: d) = (a &&# c)  :*: (b &&# d)
  (a :*: b) ||#  (c :*: d) = (a ||# c)  :*: (b ||# d)
  gnot (a :*: b) = gnot a :*: gnot b
  gall p xs = gall id ls :*: gall id rs
    where (ls, rs) = gunzip . map p . toList $ xs
  gany p xs = gany id ls :*: gany id rs
    where (ls, rs) = gunzip . map p . toList $ xs
  gxor (a :*: b) (c :*: d) = gxor a c :*: gxor b d

instance Boolean a => GBoolean (K1 i a) where
  gbool = K1 . bool
  K1 a &&# K1 b = K1 (a && b)
  K1 a ||# K1 b = K1 (a || b)
  gnot (K1 a) = K1 (not a)
  gall p as = K1 (all (unK1 . p) as)
  gany p as = K1 (any (unK1 . p) as)
  gxor (K1 a) (K1 b) = K1 (xor a b)

instance GBoolean a => GBoolean (M1 i c a) where
  gbool = M1 . gbool
  M1 a &&# M1 b = M1 (a &&# b)
  M1 a ||# M1 b = M1 (a ||# b)
  gnot (M1 a) = M1 (gnot a)
  gall p as = M1 (gall (unM1 . p) as)
  gany p as = M1 (gany (unM1 . p) as)
  gxor (M1 a) (M1 b) = M1 (gxor a b)

-- | The normal 'Bool' operators in Haskell are not overloaded. This
-- provides a richer set that are.
--
-- Instances for this class for product-like types can be automatically derived
-- for any type that is an instance of 'Generic'
class Boolean b where
  -- | Lift a 'Bool'
  bool :: Bool -> b
  -- |
  -- @'true' = 'bool' 'True'@
  true :: b
  true = bool True
  -- |
  -- @'false' = 'bool' 'False'@
  false :: b
  false = bool False

  -- | Logical conjunction.
  (&&) :: b -> b -> b

  -- | Logical disjunction (inclusive or).
  (||) :: b -> b -> b

  -- | Logical implication.
  (==>) :: b -> b -> b
  x ==> y = not x || y

  -- | Logical negation
  not :: b -> b

  -- | The logical conjunction of several values.
  and :: Foldable t => t b -> b
  and = Ersatz.Bit.all id  -- qualified for HLint

  -- | The logical disjunction of several values.
  or :: Foldable t => t b -> b
  or = Ersatz.Bit.any id  -- qualified for HLint

  -- | The negated logical conjunction of several values.
  --
  -- @'nand' = 'not' . 'and'@
  nand :: Foldable t => t b -> b
  nand = not . and

  -- | The negated logical disjunction of several values.
  --
  -- @'nor' = 'not' . 'or'@
  nor :: Foldable t => t b -> b
  nor = not . or

  -- | The logical conjunction of the mapping of a function over several values.
  all :: (Foldable t, Boolean b) => (a -> b) -> t a -> b

  -- | The logical disjunction of the mapping of a function over several values.
  any :: (Foldable t, Boolean b) => (a -> b) -> t a -> b

  -- | Exclusive-or
  xor :: b -> b -> b

  -- | Choose between two alternatives based on a selector bit.
  choose :: b  -- ^ False branch
         -> b  -- ^ True branch
         -> b  -- ^ Predicate/selector branch
         -> b
  choose f t s = (f && not s) || (t && s)

#ifndef HLINT
  default bool :: (Generic b, GBoolean (Rep b)) => Bool -> b
  bool = to . gbool

  default (&&) :: (Generic b, GBoolean (Rep b)) => b -> b -> b
  x && y = to (from x &&# from y)

  default (||) :: (Generic b, GBoolean (Rep b)) => b -> b -> b
  x || y = to (from x ||# from y)

  default not :: (Generic b, GBoolean (Rep b)) => b -> b
  not = to . gnot . from

  default all :: (Foldable t, Generic b, GBoolean (Rep b)) => (a -> b) -> t a -> b
  all p = to . gall (from . p)

  default any :: (Foldable t, Generic b, GBoolean (Rep b)) => (a -> b) -> t a -> b
  any p = to . gany (from . p)

  default xor :: (Generic b, GBoolean (Rep b)) => b -> b -> b
  xor x y = to (from x `gxor` from y)
#endif

instance Boolean Bool where
  bool = id
  true = True
  false = False

  (&&) = (Prelude.&&)
  (||) = (Prelude.||)

  not = Prelude.not

  and = Foldable.and
  or  = Foldable.or

  all = Foldable.all
  any = Foldable.any

  xor = (/=)

  choose f _ False = f
  choose _ t True  = t

gunzip :: [(f :*: g) a] -> ([f a], [g a])
gunzip = foldr go ([],[])
  where go (a :*: b) ~(as, bs) = (a:as, b:bs)
