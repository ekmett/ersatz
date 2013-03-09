{-# LANGUAGE Rank2Types, GeneralizedNewtypeDeriving, TypeOperators, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, PatternGuards, DeriveDataTypeable, DefaultSignatures, TypeFamilies, BangPatterns #-}
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
module Ersatz.Internal.Problem
  (
  -- * Implementation Details
  -- ** QDIMACS encoding
    QDIMACS(..)
  -- ** Formulas
  , Problem(qbfLastAtom, qbfFormula, qbfUniversals, qbfSNMap), emptyProblem
  , Formula(..), Clause(..), clauseLiterals
  , formulaEmpty, formulaLiteral
  , formulaNot, formulaAnd, formulaOr, formulaXor, formulaMux
  ) where

import Control.Applicative
import Control.Monad.State
import Data.Default
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.List as List (groupBy, intersperse)
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable
import Ersatz.Internal.Literal
import Ersatz.Internal.StableName
import System.IO.Unsafe

-- | (Q)QDIMACS file format pretty printer
--
-- This is used to generate the problem statement for a given 'SAT' 'Ersatz.Solver.Solver'.
class QDIMACS t where
  qdimacs :: t -> String

instance QDIMACS Literal where
  qdimacs (Literal n) = show n

-- | A (quantified) boolean formula.
data Problem = Problem
  { qbfLastAtom   :: {-# UNPACK #-} !Int      -- ^ The id of the last atom allocated
  , qbfFormula    :: !Formula                 -- ^ a set of clauses to assert
  , qbfUniversals :: !IntSet                  -- ^ a set indicating which literals are universally quantified
  , qbfSNMap      :: !(HashMap (StableName ()) Literal)  -- ^ a mapping used during 'Bit' expansion
  -- , qbfNameMap    :: !(IntMap String)      -- ^ a map of literals to given names
  } deriving Typeable

-- TODO: instance Monoid Problem

instance Default Problem where
  def = emptyProblem

-- | The trivial quantified boolean formula with no constraints.
emptyProblem :: Problem
emptyProblem = Problem 0 (Formula Set.empty) IntSet.empty HashMap.empty

newtype Formula = Formula { formulaSet :: Set Clause }
  deriving (Eq, Ord, Monoid, Typeable)

newtype Clause = Clause { clauseSet :: IntSet }
  deriving (Eq, Ord, Monoid, Typeable)

instance Show Problem where
  showsPrec p qbf = showParen (p > 10)
                  $ showString "Problem " . showsPrec 11 (qbfFormula qbf)

instance Show Formula where
  showsPrec p = showParen (p > 2) . foldr (.) id
              . List.intersperse (showString " & ") . map (showsPrec 3)
              . Set.toList . formulaSet

instance Show Clause where
  showsPrec p = showParen (p > 1) . foldr (.) id
              . List.intersperse (showString " | ") . map (showsPrec 2)
              . IntSet.toList . clauseSet

instance QDIMACS Problem where
  qdimacs (Problem vars formula@(Formula cs) qs _) =
    unlines (header : map showGroup quantGroups) ++ qdimacs formula
    where
      header = unwords ["p", "cnf", show (vars + padding), show (Set.size cs) ]

      -- "The innermost quantified set is always of type 'e'" per QDIMACS standard
      padding | Just (n, _) <- IntSet.maxView qs, n == vars = 1
              | otherwise                                   = 0
                    -- no universals means we are a plan DIMACS file
      quantGroups | IntSet.null qs = []
                    -- otherwise, skip to the first universal and show runs
                  | otherwise = List.groupBy eqQuant $ quants [head qlist..vars] qlist
        where qlist = IntSet.toAscList qs

      showGroup :: [Quant] -> String
      showGroup xs = unwords $ q (head xs) : map (show . getQuant) xs

      eqQuant :: Quant -> Quant -> Bool
      eqQuant Exists{} Exists{} = True
      eqQuant Forall{} Forall{} = True
      eqQuant _ _ = False

      q :: Quant -> String
      q Exists{} = "e"
      q Forall{} = "a"

      quants :: [Int] -> [Int] -> [Quant]
      quants [] _ = []
      quants (i:is) []     = Exists i : quants is []
      quants (i:is) jjs@(j:js)
        | i == j    = Forall i : quants is js
        | otherwise = Exists i : quants is jjs

-- | An explicit prenex quantifier
data Quant
  = Exists { getQuant :: {-# UNPACK #-} !Int }
  | Forall { getQuant :: {-# UNPACK #-} !Int }
  deriving Typeable

instance QDIMACS Formula where
  qdimacs (Formula cs) = unlines $ map qdimacs (Set.toList cs)

instance QDIMACS Clause where
  qdimacs (Clause xs) = unwords $ map show (IntSet.toList xs) ++ ["0"]

-- | Extract the (possibly negated) atoms referenced by a 'Clause'.
clauseLiterals :: Clause -> [Literal]
clauseLiterals (Clause is) = Literal <$> IntSet.toList is

-- | A formula with no clauses
formulaEmpty :: Formula
formulaEmpty = Formula Set.empty

-- | Assert a literal
formulaLiteral :: Literal -> Formula
formulaLiteral (Literal l) =
  Formula (Set.singleton (Clause (IntSet.singleton l)))

-- | The boolean /not/ operation
--
-- @
-- O ≡ ¬A
-- (O → ¬A) & (¬O → A)
-- (¬O | ¬A) & (O | A)
-- @
formulaNot :: Literal  -- ^ Output
           -> Literal  -- ^ Input
           -> Formula
formulaNot (Literal out) (Literal inp) = formulaFromList cls
  where
    cls = [ [-out, -inp], [out, inp] ]

-- | The boolean /and/ operation
--
-- @
-- O ≡ (A & B & C)
-- (O → (A & B & C)) & (¬O → ¬(A & B & C))
-- (¬O | (A & B & C)) & (O | ¬(A & B & C))
-- (¬O | A) & (¬O | B) & (¬O | C) & (O | ¬A | ¬B | ¬C)
-- @
formulaAnd :: Literal    -- ^ Output
           -> [Literal]  -- ^ Inputs
           -> Formula
formulaAnd (Literal out) inpLs = formulaFromList cls
  where
    cls = (out : map negate inps) : map (\inp -> [-out, inp]) inps
    inps = map literalId inpLs

-- | The boolean /or/ operation
--
-- @
-- O ≡ (A | B | C)
-- (O → (A | B | C)) & (¬O → ¬(A | B | C))
-- (¬O | (A | B | C)) & (O | ¬(A | B | C))
-- (¬O | A | B | C) & (O | (¬A & ¬B & ¬C))
-- (¬O | A | B | C) & (O | ¬A) & (O | ¬B) & (O | ¬C)
-- @
formulaOr :: Literal    -- ^ Output
          -> [Literal]  -- ^ Inputs
          -> Formula
formulaOr (Literal out) inpLs = formulaFromList cls
  where
    cls = (-out : inps)
        : map (\inp -> [out, -inp]) inps
    inps = map literalId inpLs

-- | The boolean /xor/ operation
--
-- @
-- O ≡ A ⊕ B
-- O ≡ ((¬A & B) | (A & ¬B))
-- (O → ((¬A & B) | (A & ¬B))) & (¬O → ¬((¬A & B) | (A & ¬B)))
-- @
--
-- Left hand side:
-- @
-- O → ((¬A & B) | (A & ¬B))
-- ¬O | ((¬A & B) | (A & ¬B))
-- ¬O | ((¬A | A) & (¬A | ¬B) & (A | B) & (¬B | B))
-- ¬O | ((¬A | ¬B) & (A | B))
-- (¬O | ¬A | ¬B) & (¬O | A | B)
-- @
--
-- Right hand side:
--
-- @
-- ¬O → ¬((¬A & B) | (A & ¬B))
-- O | ¬((¬A & B) | (A & ¬B))
-- O | (¬(¬A & B) & ¬(A & ¬B))
-- O | ((A | ¬B) & (¬A | B))
-- (O | ¬A | B) & (O | A | ¬B)
-- @
--
-- Result:
--
-- @
-- (¬O | ¬A | ¬B) & (¬O | A | B) & (O | ¬A | B) & (O | A | ¬B)
-- @
formulaXor :: Literal  -- ^ Output
           -> Literal  -- ^ Input
           -> Literal  -- ^ Input
           -> Formula
formulaXor (Literal out) (Literal inpA) (Literal inpB) = formulaFromList cls
  where
    cls = [ [-out, -inpA, -inpB]
          , [-out,  inpA,  inpB]
          , [ out, -inpA,  inpB]
          , [ out,  inpA, -inpB]
          ]

-- | The boolean /else-then-if/ or /mux/ operation
--
-- @
-- O ≡ (F & ¬P) | (T & P)
-- (O → ((F & ¬P) | (T & P))) & (¬O → ¬((F & ¬P) | (T & P)))
-- @
--
-- Left hand side:
--
-- @
-- O → ((F & ¬P) | (T & P))
-- ¬O | ((F & ¬P) | (T & P))
-- ¬O | ((F | T) & (F | P) & (T | ¬P) & (¬P | P))
-- ¬O | ((F | T) & (F | P) & (T | ¬P))
-- (¬O | F | T) & (¬O | F | P) & (¬O | T | ¬P)
-- @
--
-- Right hand side:
--
-- @
-- ¬O → ¬((F & ¬P) | (T & P))
-- O | ¬((F & ¬P) | (T & P))
-- O | (¬(F & ¬P) & ¬(T & P))
-- O | ((¬F | P) & (¬T | ¬P))
-- (O | ¬F | P) & (O | ¬T | ¬P)
-- @
--
-- Result:
--
-- @
-- (¬O | F | T) & (¬O | F | P) & (¬O | T | ¬P) & (O | ¬F | P) & (O | ¬T | ¬P)
-- @
formulaMux :: Literal  -- ^ Output
           -> Literal  -- ^ False branch
           -> Literal  -- ^ True branch
           -> Literal  -- ^ Predicate/selector
           -> Formula
formulaMux (Literal out) (Literal inpF) (Literal inpT) (Literal inpP) =
  formulaFromList cls
  where
    cls = [ [-out,  inpF,  inpT]
          , [-out,  inpF,  inpP]
          , [-out,  inpT, -inpP]
          , [ out, -inpF,  inpP]
          , [ out, -inpT, -inpP]
          ]

formulaFromList :: [[Int]] -> Formula
formulaFromList = Formula . Set.fromList . map (Clause . IntSet.fromList)
