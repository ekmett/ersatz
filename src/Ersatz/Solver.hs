--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2010-2013, Johan Kiviniemi 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ersatz.Solver
  ( module Ersatz.Solver.DepQBF
  , module Ersatz.Solver.Minisat
  , solveWith
  ) where

import Control.Monad.State
import Data.Default
import Ersatz.Decoding
import Ersatz.Problem
import Ersatz.Solution
import Ersatz.Solver.DepQBF
import Ersatz.Solver.Minisat

solveWith :: (Monad m, HasSAT s, Default s, Decoding a) => Solver s m -> StateT s m a -> m (Result, Maybe (Decoded a))
solveWith solver m = do
  (a, problem) <- runStateT m def
  (res, litMap) <- solver problem
  return (res, decode (solutionFrom litMap problem) a)
