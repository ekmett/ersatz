{-# LANGUAGE Rank2Types, ImpredicativeTypes #-}
module Data.Logic.Ersatz.Solution
  ( Solution -- (..)
  , Result(..)
  , Solver
  , Witness(..)
  ) where

import Data.Ix
import Data.Logic.Ersatz.Internal.Problem
import Data.IntMap (IntMap)

type Solution = IntMap Bool

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

type Solver m = QBF -> m (Result, Solution)

data Witness a = Witness
  { witnessResult   :: !Result
  , witnessSolution :: Solution
  , witnessContents :: a }
