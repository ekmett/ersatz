{-# LANGUAGE Rank2Types, ImpredicativeTypes #-}
module Data.Logic.Ersatz.Solution
  ( Solution(..), solutionFrom
  , Result(..)
  , Solver
  , Witness(..)
  ) where

import qualified Data.HashMap.Lazy as HashMap
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Ix
import Control.Applicative
import System.Mem.StableName (StableName)

import Data.Logic.Ersatz.Internal.Problem

data Solution = Solution { solLookupLiteral :: Literal -> Maybe Bool
                         , solLookupSN      :: StableName () -> Maybe Bool
                         }

solutionFrom :: IntMap Bool -> QBF -> Solution
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
  toEnum _ = error "Enum.toEnum {Data.Logic.Ersatz.Solution.Result}: argument of out range"

instance Bounded Result where
  minBound = Unsolved
  maxBound = Satisfied

type Solver m = QBF -> m (Result, IntMap Bool)

data Witness a = Witness
  { witnessResult   :: !Result
  , witnessSolution :: Solution
  , witnessContents :: a }
