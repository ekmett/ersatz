--------------------------------------------------------------------
-- |
-- Copyright :  © Edward Kmett 2010-2014, Johan Kiviniemi 2013
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

import Control.Lens
import qualified Data.HashMap.Lazy as HashMap
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Ix
import Ersatz.Internal.Literal
import Ersatz.Problem
import System.Mem.StableName (StableName)

data Solution = Solution
  { solutionLiteral    :: Literal -> Maybe Bool
    -- ^ If a 'Literal' is uniquely assigned to a particular value, this will
    -- return 'Just' of that value. If a 'Literal' is unconstrained (i.e., it
    -- can be assigned either 'True' or 'False'), this will return 'Nothing'.
  , solutionStableName :: StableName () -> Maybe Bool
  }

solutionFrom :: HasSAT s => IntMap Bool -> s -> Solution
solutionFrom litMap qbf = Solution lookupLit lookupSN
  where
    lookupLit l | i >= 0    = IntMap.lookup i litMap
                | otherwise = not <$> IntMap.lookup (-i) litMap
      where i = literalId l

    lookupSN sn = lookupLit =<< HashMap.lookup sn snMap

    snMap = qbf^.stableMap

data Result
  = Unsolved
  | Unsatisfied
  | Satisfied
  deriving (Eq,Ord,Ix,Show,Read)

instance Enum Result where
  fromEnum Unsolved = -1
  fromEnum Unsatisfied = 0
  fromEnum Satisfied = 1

  toEnum (-1) = Unsolved
  toEnum 0 = Unsatisfied
  toEnum 1 = Satisfied
  toEnum _ = error "Enum.toEnum {Ersatz.Solution.Result}: argument of out range"

instance Bounded Result where
  minBound = Unsolved
  maxBound = Satisfied

-- | A @'Solver' s m@ is responsible for invoking a solver and
-- returning a 'Result' and a map of determined results.
--
-- * @s@ is typically 'SAT' or 'QSAT'
--
-- * @m@ is typically 'IO'
type Solver s m = s -> m (Result, IntMap Bool)
