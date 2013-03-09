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
module Ersatz.Problem
  (
  -- * Formulas
    Problem(qbfLastAtom, qbfFormula, qbfUniversals, qbfSNMap)
  -- * QDIMACS pretty printing
  , QDIMACS(..)
  ) where

import Data.Default
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.List as List (groupBy)
import qualified Data.Set as Set
import Data.Typeable
import Ersatz.Internal.Formula
import Ersatz.Internal.Literal
import Ersatz.Internal.StableName

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
  def = Problem 0 (Formula Set.empty) IntSet.empty HashMap.empty

instance Show Problem where
  showsPrec p qbf = showParen (p > 10)
                  $ showString "Problem .. " . showsPrec 11 (qbfFormula qbf) . showString " .."

-- | (Q)QDIMACS file format pretty printer
--
-- This is used to generate the problem statement for a given 'SAT' 'Ersatz.Solver.Solver'.
class QDIMACS t where
  qdimacs :: t -> String

instance QDIMACS Literal where
  qdimacs (Literal n) = show n

instance QDIMACS Formula where
  qdimacs (Formula cs) = unlines $ map qdimacs (Set.toList cs)

instance QDIMACS Clause where
  qdimacs (Clause xs) = unwords $ map show (IntSet.toList xs) ++ ["0"]

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

