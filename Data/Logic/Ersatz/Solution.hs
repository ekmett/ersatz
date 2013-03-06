{-# LANGUAGE Rank2Types, ImpredicativeTypes #-}
module Data.Logic.Ersatz.Solution
  ( Solution(..)
  , Result(..)
  , Solver
  , Witness(..)
  ) where

import Data.HashMap.Lazy (HashMap)
import Data.IntMap (IntMap)
import Data.Ix
import System.Mem.StableName (StableName)

import Data.Logic.Ersatz.Internal.Problem

data Solution = Solution { solLitMap :: IntMap Bool
                         , solSNMap  :: HashMap (StableName ()) Bool
                         }

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
