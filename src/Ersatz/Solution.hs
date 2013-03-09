{-# LANGUAGE Rank2Types, ImpredicativeTypes, DeriveDataTypeable #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2010-2013, Johan Kiviniemi 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ersatz.Solution
  ( Solution(..), solutionFrom
  , Result(..)
  , Solver
  ) where

import Control.Applicative
import qualified Data.HashMap.Lazy as HashMap
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Ix
import Data.Typeable
import Ersatz.Internal.Literal
import Ersatz.Problem
import System.Mem.StableName (StableName)

data Solution = Solution
  { solutionLiteral    :: Literal -> Maybe Bool
  , solutionStableName :: StableName () -> Maybe Bool
  } deriving Typeable

solutionFrom :: IntMap Bool -> Problem -> Solution
solutionFrom litMap qbf = Solution lookupLit lookupSN
  where
    lookupLit l | i >= 0    = IntMap.lookup i litMap
                | otherwise = not <$> IntMap.lookup (-i) litMap
      where i = literalId l

    lookupSN sn = lookupLit =<< HashMap.lookup sn snMap

    snMap = qbfSNMap qbf

data Result
  = Unsolved
  | Unsatisfied
  | Satisfied
  deriving (Eq,Ord,Ix,Show,Read)

instance Enum Result where
  fromEnum Unsolved = (-1)
  fromEnum Unsatisfied = 0
  fromEnum Satisfied = 1

  toEnum (-1) = Unsolved
  toEnum 0 = Unsatisfied
  toEnum 1 = Satisfied
  toEnum _ = error "Enum.toEnum {Ersatz.Solution.Result}: argument of out range"

instance Bounded Result where
  minBound = Unsolved
  maxBound = Satisfied

type Solver m = Problem -> m (Result, IntMap Bool)
