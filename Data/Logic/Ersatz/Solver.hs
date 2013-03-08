module Data.Logic.Ersatz.Solver
  ( module Data.Logic.Ersatz.Solver.Minisat
  , solveWith
  ) where

import Control.Applicative

import Data.Logic.Ersatz.Encoding
import Data.Logic.Ersatz.Problem
import Data.Logic.Ersatz.Solution
import Data.Logic.Ersatz.Solver.Minisat

solveWith :: Encoding a => Solver IO -> SAT a -> IO (Result, Maybe (Decoded a))
solveWith solver sat = do
  (a, qbf) <- satToIO sat
  (res, litMap) <- solver qbf
  (,) res <$> decode (solutionFrom litMap qbf) a
