{-# LANGUAGE OverloadedStrings #-}

module Ersatz.Solver.Lingeling
  ( lingeling
  , plingeling
  , treengeling
  , lingelingPath
  , plingelingPath
  , treengelingPath
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

-- | 'Solver' for 'SAT' problems that tries to invoke the @lingeling@ executable
-- from the @PATH@.
lingeling :: MonadIO m => Solver SAT m
lingeling = lingelingPath "lingeling"

-- | 'Solver' for 'SAT' problems that tries to invoke the @plingeling@ executable
-- from the @PATH@.
plingeling :: MonadIO m => Solver SAT m
plingeling = ngelingPath "plingeling"

-- | 'Solver' for 'SAT' problems that tries to invoke the @treengeling@ executable
-- from the @PATH@.
treengeling :: MonadIO m => Solver SAT m
treengeling = ngelingPath "treengeling"

-- | 'Solver' for 'SAT' problems that tries to invoke a program that takes
-- @lingeling@ compatible arguments.
--
-- The 'FilePath' refers to the path to the executable.
lingelingPath :: MonadIO m => FilePath -> Solver SAT m
lingelingPath = ngelingPath

-- | 'Solver' for 'SAT' problems that tries to invoke a program that takes
-- @plingeling@ compatible arguments.
--
-- The 'FilePath' refers to the path to the executable.
plingelingPath :: MonadIO m => FilePath -> Solver SAT m
plingelingPath = ngelingPath

-- | 'Solver' for 'SAT' problems that tries to invoke a program that takes
-- @treengeling@ compatible arguments.
--
-- The 'FilePath' refers to the path to the executable.
treengelingPath :: MonadIO m => FilePath -> Solver SAT m
treengelingPath = ngelingPath

-- | 'Solver' for 'SAT' problems that tries to invoke a program that takes
-- @*ngeling@ compatible arguments.
--
-- The 'FilePath' refers to the path to the executable.
ngelingPath :: MonadIO m => FilePath -> Solver SAT m
ngelingPath path problem = liftIO $
  withTempFiles ".cnf" "" $ \problemPath _ -> do
    writeDimacs' problemPath problem

    (exit, out, _err) <-
      readProcessWithExitCode path
                              [problemPath]
                              []

    let sol = parseSolution5 out

    return (resultOf exit, sol)
