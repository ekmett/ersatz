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

import Data.Default
import Ersatz.Decoding
import Ersatz.Monad
import Ersatz.Solution
import Ersatz.Solver.DepQBF
import Ersatz.Solver.Minisat

solveWith :: (Monad m, Decoding a) => Solver m -> SAT m a -> m (Result, Maybe (Decoded a))
solveWith solver m = do
  (a, qbf) <- runSAT m (\a s -> return (a , s)) def
  (res, litMap) <- solver qbf
  return (res, decode (solutionFrom litMap qbf) a)
