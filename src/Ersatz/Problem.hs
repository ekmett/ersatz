{-# LANGUAGE Rank2Types, GeneralizedNewtypeDeriving, TypeOperators, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, PatternGuards, DeriveDataTypeable, DefaultSignatures, TypeFamilies, BangPatterns, TemplateHaskell #-}
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
  -- * SAT
    SAT(SAT)
  , HasSAT(..)
  , literalExists
  , assertFormula
  , generateLiteral
  -- * QSAT
  , QSAT(QSAT)
  , HasQSAT(..)
  , literalForall
  -- * QDIMACS pretty printing
  , QDIMACS(..)
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.State.Class
import Data.Default
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.List as List (groupBy)
import Data.Monoid
import qualified Data.Set as Set
import Data.Typeable
import Ersatz.Internal.Formula
import Ersatz.Internal.Literal
import Ersatz.Internal.StableName
import System.IO.Unsafe

------------------------------------------------------------------------------
-- SAT Problems
------------------------------------------------------------------------------

data SAT = SAT
  { _lastAtom  :: {-# UNPACK #-} !Int      -- ^ The id of the last atom allocated
  , _formula   :: !Formula                 -- ^ a set of clauses to assert
  , _stableMap :: !(HashMap (StableName ()) Literal)  -- ^ a mapping used during 'Bit' expansion
  } deriving Typeable

makeLensesWith ?? ''SAT $ classyRules & lensClass.mapped ?~ ("HasSAT","sat")


instance Show SAT where
  showsPrec p bf = showParen (p > 10)
                 $ showString "SAT " . showsPrec 11 (bf^.lastAtom) . showsPrec 11 (bf^.formula) . showString " mempty"


instance Default SAT where
  def = SAT 0 (Formula Set.empty) HashMap.empty

literalExists :: (MonadState s m, HasSAT s) => m Literal
literalExists = liftM Literal $ lastAtom <+= 1
{-# INLINE literalExists #-}

assertFormula :: (MonadState s m, HasSAT s) => Formula -> m ()
assertFormula xs = formula <>= xs
{-# INLINE assertFormula #-}

generateLiteral :: (MonadState s m, HasSAT s) => a -> (Literal -> m ()) -> m Literal
generateLiteral a f = do
  let sn = unsafePerformIO (makeStableName' a)
  use (stableMap.at sn) >>= \ ml -> case ml of
    Just l -> return l
    Nothing -> do
      l <- literalExists
      stableMap.at sn ?= l
      f l
      return l
{-# INLINE generateLiteral #-}

------------------------------------------------------------------------------
-- QSAT Problems
------------------------------------------------------------------------------

-- | A (quantified) boolean formula.
data QSAT = QSAT
  { _universals :: !IntSet -- ^ a set indicating which literals are universally quantified
  , _qsatSat    :: SAT     -- ^ The rest of the information, in 'SAT'
  } deriving (Show,Typeable)

class HasSAT t => HasQSAT t where
  qsat       :: Lens' t QSAT
  universals :: Lens' t IntSet
  universals f = qsat ago where
    ago (QSAT u s) = f u <&> \u' -> QSAT u' s

instance HasSAT QSAT where
  sat f (QSAT u s) = QSAT u <$> f s

instance HasQSAT QSAT where
  qsat = id

instance Default QSAT where
  def = QSAT IntSet.empty def

literalForall :: (MonadState s m, HasQSAT s) => m Literal
literalForall = do
   l <- lastAtom <+= 1
   universals.contains l .= True
   return $ Literal l
{-# INLINE literalForall #-}

------------------------------------------------------------------------------
-- Printing QSATs
------------------------------------------------------------------------------

-- | (Q)DIMACS file format pretty printer
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

instance QDIMACS SAT where
  -- for now this is backwards. TODO: flip it around
  qdimacs xs = qdimacs (QSAT mempty xs)

instance QDIMACS QSAT where
  qdimacs (QSAT qs (SAT vars f@(Formula cs) _)) =
    unlines (header : map showGroup quantGroups) ++ qdimacs f
    where
      header = unwords ["p", "cnf", show (vars + padding), show (Set.size cs) ]

      -- "The innermost quantified set is always of type 'e'" per QDIMACS standard
      padding | Just (n, _) <- IntSet.maxView qs, n == vars = 1
              | otherwise                                   = 0
                    -- no universals means we are a plain DIMACS file
      quantGroups | IntSet.null qs = []
                    -- otherwise, skip to the first universal and show runs
                  | otherwise = List.groupBy eqQuant $ quants [head qlist..vars] qlist
        where qlist = IntSet.toAscList qs

      showGroup :: [Quant] -> String
      showGroup xs = unwords $ q (head xs) : map (show . getQuant) xs ++ ["0"]

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

