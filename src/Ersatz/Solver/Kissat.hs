{-# LANGUAGE OverloadedStrings #-}

module Ersatz.Solver.Kissat
  ( kissat
  , kissatPath
  ) where

import Control.Monad.IO.Class
  ( MonadIO ( liftIO
            )
  )
import Ersatz.Problem
  ( SAT
  , writeDimacs'
  )
import Ersatz.Solution
  ( Solver
  )
import Ersatz.Solver.Common
  ( resultOf
  , withTempFiles
  , parseSolution5
  )
import System.Process
  ( readProcessWithExitCode
  )


-- | 'Solver' for 'SAT' problems that tries to invoke the @kissat@ executable
-- from the @PATH@.
kissat :: MonadIO m => Solver SAT m
kissat = kissatPath "kissat"

-- | 'Solver' for 'SAT' problems that tries to invoke a program that takes
-- @kissat@ compatible arguments.
--
-- The 'FilePath' refers to the path to the executable.
kissatPath :: MonadIO m => FilePath -> Solver SAT m
kissatPath path problem = liftIO $
  withTempFiles ".cnf" "" $ \problemPath _ -> do
    writeDimacs' problemPath problem

    (exit, out, _err) <-
      readProcessWithExitCode path
                              [problemPath]
                              []

    let sol = parseSolution5 out

    return (resultOf exit, sol)
