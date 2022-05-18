{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_HADDOCK not-home #-}
--------------------------------------------------------------------
-- |
-- Copyright :  © Edward Kmett 2010-2014, Johan Kiviniemi 2013
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
import Control.Monad (MonadPlus(..), liftM2, liftM3)
import Data.Foldable (toList)
import qualified Data.Foldable as Foldable
import qualified Data.Traversable as Traversable
import Data.Sequence (Seq, (<|), (|>), (><))
import qualified Data.Sequence as Seq
import Ersatz.Codec
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
data Bit
  = And !(Seq Bit)
  | Xor !Bit !Bit
  | Mux !Bit !Bit !Bit
  | Not !Bit
  | Var !Literal
  | Run ( forall m s . MonadSAT s m => m Bit )

instance Show Bit where
  showsPrec d (And xs)  = showParen (d > 10) $
    showString "And " . showsPrec 11 xs
  showsPrec d (Xor x y) = showParen (d > 10) $
    showString "Xor " . showsPrec 11 x . showChar ' ' . showsPrec 11 y
  showsPrec d (Mux x y z) = showParen (d > 10) $
    showString "Mux " . showsPrec 11 x . showChar ' ' . showsPrec 11 y . showChar ' ' . showsPrec 11 z
  showsPrec d (Not x)  = showParen (d > 10) $ showString "Not " . showsPrec 11 x
  showsPrec d (Var x)  = showParen (d > 10) $ showString "Var " . showsPrec 11 x
  showsPrec d (Run _)  = showParen (d > 10) $ showString "Run ..."

instance Boolean Bit where
  -- improve the stablemap this way
  bool True  = true
  bool False = false
  true  = Var literalTrue
  false = Var literalFalse

  (&&) = and2
  a || b = not (not a && not b)

  not (Not c) = c
  not (Var l) = Var (negateLiteral l)
  not c       = Not c

  a `xor` Var (Literal (-1)) = a
  Var (Literal (-1)) `xor` b = b
  a `xor` Var (Literal 1) = not a
  Var (Literal 1) `xor` b = not b
  -- following 3 clauses might enable some AIG magic
  Not a `xor` Not b = Xor a b
  a `xor` Not b = Not (Xor a b)
  Not a `xor` b = Not (Xor a b)
  a `xor` b    = Xor a b

  and = Foldable.foldl' (&&) true
  or  = not . all not . toList

  all p = Foldable.foldl' (\res b -> res && p b) true
  any p = not . all (not . p)

  choose f _ (Var (Literal (-1))) = f
  choose _ t (Var (Literal 1))    = t
  choose t f (Not s) = choose f t s
  choose f t s = Mux f t s

and2 :: Bit -> Bit -> Bit
and2 a@(Var (Literal (-1))) _ = a
and2 _ b@(Var (Literal (-1))) = b
and2 a (Var (Literal 1)) = a
and2 (Var (Literal 1)) b = b
and2 (And as) (And bs) = And (as >< bs)
and2 (And as) b      = And (as |> b)
and2 a (And bs) = And (a <| bs)
and2 a b = And (a <| b <| Seq.empty)

instance Variable Bit where
  literally = fmap Var . literally

-- a Bit you don't assert is actually a boolean function that you can evaluate later after compilation
instance Codec Bit where
  type Decoded Bit = Bool
  encode = bool
  decode sol b
      = maybe (pure False <|> pure True) pure $
        solutionStableName sol (unsafePerformIO (makeStableName' b))
     -- The StableName didn’t have an associated literal with a solution,
     -- recurse to compute the value.
    <|> case b of
          And cs  -> andMaybeBools . toList $ decode sol <$> cs
          Xor x y -> xor <$> decode sol x <*> decode sol y
          Mux cf ct cp -> do
            p <- decode sol cp
            decode sol $ if p then ct else cf
          Not c'  -> not <$> decode sol c'
          Var l   -> decode sol l
          Run _ -> mzero
    where
      andMaybeBools :: [Maybe Bool] -> Maybe Bool
      andMaybeBools mbs
        | any not knowns = Just False  -- One is known to be false.
        | null unknowns  = Just True   -- All are known to be true.
        | otherwise      = Nothing     -- Unknown.
        where
          (unknowns, knowns) = partitionMaybes mbs

      partitionMaybes :: [Maybe a] -> ([()], [a])
      partitionMaybes = foldr go ([],[])
        where
          go Nothing  ~(ns, js) = (():ns, js)
          go (Just a) ~(ns, js) = (ns,    a:js)

-- | Assert claims that 'Bit' must be 'true' in any satisfying interpretation
-- of the current problem.
assert :: MonadSAT s m => Bit -> m ()
assert (And bs) = Foldable.for_ bs assert
-- the following (when switched on) produces extra clauses, why?
assert (Not (And bs)) | () /= () = do
  ls <- Traversable.for bs runBit
  assertFormula $ fromClause $ foldMap (fromLiteral . negateLiteral) ls
assert b = do
  l <- runBit b
  assertFormula (formulaLiteral l)

-- | Convert a 'Bit' to a 'Literal'.
runBit :: MonadSAT s m => Bit -> m Literal
runBit (Not c) = negateLiteral `fmap` runBit c
runBit (Var l) = return l
runBit (Run action) = action >>= runBit
runBit b = generateLiteral b $ \out ->
  assertFormula =<< case b of
    And bs    -> formulaAnd out `fmap` mapM runBit (toList bs)
    Xor x y   -> liftM2 (formulaXor out) (runBit x) (runBit y)
    Mux x y p -> liftM3 (formulaMux out) (runBit x) (runBit y) (runBit p)

#if __GLASGOW_HASKELL__ < 900
    -- Already handled above, but pre-9.0 GHCs don't realize this.
    Not _ -> error "Unreachable"
    Var _ -> error "Unreachable"
    Run _ -> error "Unreachable"
#endif

class GBoolean f where
  gbool :: Bool -> f a
  (&&#) :: f a -> f a -> f a
  (||#) :: f a -> f a -> f a
  gnot :: f a -> f a
  gall :: Foldable t => (a -> f b) -> t a -> f b
  gany :: Foldable t => (a -> f b) -> t a -> f b
  gxor :: f a -> f a -> f a

instance GBoolean V1 where
  gbool x    = x `seq` error "GBoolean[V1].gbool"
  x &&# y    = x `seq` y `seq` error "GBoolean[V1].&&#"
  x ||# y    = x `seq` y `seq` error "GBoolean[V1].||#"
  gnot x     = x `seq` error "GBoolean[V1].gnot"
  gall x y   = x `seq` y `seq` error "GBoolean[V1].gall"
  gany x y   = x `seq` y `seq` error "GBoolean[V1].gany"
  gxor x y   = x `seq` y `seq` error "GBoolean[V1].gxor"

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
  all :: Foldable t => (a -> b) -> t a -> b

  -- | The logical disjunction of the mapping of a function over several values.
  any :: Foldable t => (a -> b) -> t a -> b

  -- | Exclusive-or
  xor :: b -> b -> b

  -- | Choose between two alternatives based on a selector bit.
  choose :: b  -- ^ False branch
         -> b  -- ^ True branch
         -> b  -- ^ Predicate/selector branch
         -> b
  choose f t s = (f && not s) || (t && s)

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
