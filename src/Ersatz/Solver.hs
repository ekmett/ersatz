--------------------------------------------------------------------
-- |
-- Copyright :  © Edward Kmett 2010-2014, Johan Kiviniemi 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ersatz.Solver
  ( module Ersatz.Solver.DepQBF
  , module Ersatz.Solver.Kissat
  , module Ersatz.Solver.Lingeling
  , module Ersatz.Solver.Minisat
  , module Ersatz.Solver.Z3
  , solveWith
  ) where

import Control.Monad.State
import Data.Default
import Ersatz.Codec
import Ersatz.Problem
import Ersatz.Solution
import Ersatz.Solver.DepQBF
import Ersatz.Solver.Kissat
import Ersatz.Solver.Lingeling
import Ersatz.Solver.Minisat
import Ersatz.Solver.Z3

-- | @'solveWith' solver prob@ solves a SAT problem @prob@ with the given
-- @solver@. It returns a pair consisting of:
--
-- 1. A 'Result' that indicates if @prob@ is satisfiable ('Satisfied'),
--    unsatisfiable ('Unsatisfied'), or if the solver could not determine any
--    results ('Unsolved').
--
-- 2. A 'Decoded' answer that was decoded using the solution to @prob@. Note
--    that this answer is only meaningful if the 'Result' is 'Satisfied' and
--    the answer value is in a 'Just'.
--
-- Here is a small example of how to use 'solveWith':
--
-- @
-- import Ersatz
--
-- main :: IO ()
-- main = do
--   res <- 'solveWith' minisat $ do
--     (b1 :: Bit) <- exists
--     (b2 :: Bit) <- exists
--     assert (b1 === b2)
--     pure [b1, b2]
--   case res of
--     (Satisfied, Just answer) -> print answer
--     _ -> fail "Could not solve problem"
-- @
--
-- Depending on the whims of @minisat@, this program may print either
-- @[False, False]@ or @[True, True]@.
solveWith ::
  (Monad m, HasSAT s, Default s, Codec a) =>
  Solver s m -> StateT s m a -> m (Result, Maybe (Decoded a))
solveWith solver m = do
  (a, problem) <- runStateT m def
  (res, litMap) <- solver problem
  return (res, decode (solutionFrom litMap problem) a)
