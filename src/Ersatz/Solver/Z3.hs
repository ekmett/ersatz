--------------------------------------------------------------------
-- |
-- Copyright :  © Edward Kmett 2010-2014, Johan Kiviniemi 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ersatz.Solver.Z3
  ( z3
  , z3Path
  ) where

import Data.ByteString.Builder
import Control.Monad.IO.Class
import Ersatz.Problem
import Ersatz.Solution
import Ersatz.Solver.Common
import System.IO
import System.Process (readProcessWithExitCode)

-- | 'Solver' for 'SAT' problems that tries to invoke the @z3@ executable from the @PATH@
z3 :: MonadIO m => Solver SAT m
z3 = z3Path "z3"

-- | 'Solver' for 'SAT' problems that tries to invoke a program that takes @z3@ compatible arguments.
--
-- The 'FilePath' refers to the path to the executable.
z3Path :: MonadIO m => FilePath -> Solver SAT m
z3Path path problem = liftIO $
  withTempFiles ".cnf" "" $ \problemPath _ -> do
    withFile problemPath WriteMode $ \fh ->
      hPutBuilder fh (dimacs problem)

    (_exit, out, _err) <-
      readProcessWithExitCode path ["-dimacs", problemPath] []

    let result = case lines out of
                    "s SATISFIABLE":_   -> Satisfied
                    "s UNSATISFIABLE":_ -> Unsatisfied
                    _                   -> Unsolved

    return (result, parseSolution5 out)
