{-# LANGUAGE Rank2Types #-}
module Data.Logic.Ersatz.Solution
    ( Solution -- (..)
    , Result(..)
    , Solver
    , Witness(..)
    ) where

import Data.Ix
import Data.Logic.Ersatz.Internal.Problem
-- import Data.IntMap (IntMap)

type Solution b = Literal b -> Maybe Bool
-- newtype Solution b = Solution { solutionMap :: IntMap Bool }

data Result
    = Unsolved
    | Unsatisfied
    | Satisfied
    deriving (Ix,Show,Read)

instance Enum Result where
    fromEnum Unsolved = (-1)
    fromEnum Unsatisfied = 0
    fromEnum Satisfied = 1
    toEnum (-1) = Unsolved
    toEnum 0 = Unsatisfied
    toEnum 1 = Satisfied
    toEnum _ = error "Enum.toEnum {Data.Logic.Ersatz.Solution.Result}: argument of out range"

instance Eq Result where
    a == b = fromEnum a == fromEnum b

instance Ord Result where
    a `compare` b = fromEnum a `compare` fromEnum b

instance Bounded Result where
    minBound = Unsolved
    maxBound = Satisfied

type Solver m = forall b. QBF b -> m (Result, Solution b)

data Witness s a = Witness
    { witnessResult   :: Result
    , witnessSolution :: !(Solution s)
    , witnessContents :: a }
