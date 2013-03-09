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
module Ersatz.Internal.Formula
  (
  -- * Clauses
    Clause(..), clauseLiterals
  -- * Formulas
  , Formula(..)
  , formulaEmpty, formulaLiteral
  , formulaNot, formulaAnd, formulaOr, formulaXor, formulaMux
  ) where

import Control.Applicative
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.List as List (intersperse)
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable
import Ersatz.Internal.Literal

------------------------------------------------------------------------------
-- Clauses
------------------------------------------------------------------------------

-- | A disjunction of possibly negated atoms. Negated atoms are represented
-- by negating the identifier.
newtype Clause = Clause { clauseSet :: IntSet }
  deriving (Eq, Ord, Monoid, Typeable)

-- | Extract the (possibly negated) atoms referenced by a 'Clause'.
clauseLiterals :: Clause -> [Literal]
clauseLiterals (Clause is) = Literal <$> IntSet.toList is

------------------------------------------------------------------------------
-- Formulas
------------------------------------------------------------------------------

-- | A conjunction of clauses
newtype Formula = Formula { formulaSet :: Set Clause }
  deriving (Eq, Ord, Monoid, Typeable)

instance Show Formula where
  showsPrec p = showParen (p > 2) . foldr (.) id
              . List.intersperse (showString " & ") . map (showsPrec 3)
              . Set.toList . formulaSet

instance Show Clause where
  showsPrec p = showParen (p > 1) . foldr (.) id
              . List.intersperse (showString " | ") . map (showsPrec 2)
              . IntSet.toList . clauseSet


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
