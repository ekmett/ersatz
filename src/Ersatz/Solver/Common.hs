--------------------------------------------------------------------
-- |
-- Copyright :  © Edward Kmett 2010-2013, Johan Kiviniemi 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ersatz.Solver.Common
  ( withTempFiles
  , resultOf
  ) where

import Control.Monad.IO.Class
import Ersatz.Solution
import System.Exit (ExitCode(..))
import System.IO.Temp (withSystemTempDirectory)

withTempFiles :: MonadIO m
              => FilePath  -- ^ Problem file extension including the dot, if any
              -> FilePath  -- ^ Solution file extension including the dot, if any
              -> (FilePath -> FilePath -> IO a) -> m a
withTempFiles problemExt solutionExt f = liftIO $
  withSystemTempDirectory "ersatz" $ \dir -> do
    let problemPath  = dir ++ "/problem"  ++ problemExt
        solutionPath = dir ++ "/solution" ++ solutionExt

    f problemPath solutionPath

resultOf :: ExitCode -> Result
resultOf (ExitFailure 10) = Satisfied
resultOf (ExitFailure 20) = Unsatisfied
resultOf _                = Unsolved
