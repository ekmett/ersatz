{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
module Ersatz.Internal.Formula
  (
  -- * Clauses
    Clause(..), clauseLiterals
  -- * Formulas
  , Formula(..)
  , formulaEmpty, formulaLiteral
  , formulaNot, formulaAnd, formulaOr, formulaXor, formulaMux
  ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.List as List (intersperse)
#if __GLASGOW_HASKELL__ < 710
import Data.Monoid
#endif
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable
import Ersatz.Internal.Literal

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Foldable (toList)

------------------------------------------------------------------------------
-- Clauses
------------------------------------------------------------------------------

-- | A disjunction of possibly negated atoms. Negated atoms are represented
-- by negating the identifier.
newtype Clause = Clause { clauseSet :: IntSet }
  deriving (Eq, Ord, Typeable)

-- | Extract the (possibly negated) atoms referenced by a 'Clause'.
clauseLiterals :: Clause -> [Literal]
clauseLiterals (Clause is) = Literal <$> IntSet.toList is

instance Monoid Clause where
  mempty = Clause mempty
  mappend (Clause x) (Clause y) = Clause (mappend x y)

------------------------------------------------------------------------------
-- Formulas
------------------------------------------------------------------------------

-- | A conjunction of clauses
newtype Formula = Formula { formulaSet :: Seq Clause }
  deriving (Eq, Ord, Typeable)

instance Monoid Formula where
  mempty = Formula mempty
  mappend (Formula x) (Formula y) = Formula (mappend x y)

instance Show Formula where
  showsPrec p = showParen (p > 2) . foldr (.) id
              . List.intersperse (showString " & ") . map (showsPrec 3)
              . Data.Foldable.toList . formulaSet

instance Show Clause where
  showsPrec p = showParen (p > 1) . foldr (.) id
              . List.intersperse (showString " | ") . map (showsPrec 2)
              . IntSet.toList . clauseSet


-- | A formula with no clauses
formulaEmpty :: Formula
formulaEmpty = mempty

-- | Assert a literal
formulaLiteral :: Literal -> Formula
formulaLiteral (Literal l) = fromClause (Clause (IntSet.singleton l))

fromClause :: Clause -> Formula
fromClause c = Formula { formulaSet = Seq.singleton c }


-- | The boolean /not/ operation
--
-- Derivation of the Tseitin transformation:
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
-- Derivation of the Tseitin transformation:
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
-- Derivation of the Tseitin transformation:
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
-- Derivation of the Tseitin transformation:
--
-- @
-- O ≡ A ⊕ B
-- O ≡ ((¬A & B) | (A & ¬B))
-- (O → ((¬A & B) | (A & ¬B))) & (¬O → ¬((¬A & B) | (A & ¬B)))
-- @
--
-- Left hand side:
--
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
-- Derivation of the Tseitin transformation:
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
formulaFromList = foldMap (  fromClause . Clause . IntSet.fromList )
