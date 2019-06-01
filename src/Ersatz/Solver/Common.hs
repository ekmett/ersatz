--------------------------------------------------------------------
-- |
-- Copyright :  © Edward Kmett 2010-2014, Johan Kiviniemi 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ersatz.Solver.Common
  ( withTempFiles
  , resultOf

  -- * Support for trying many solvers
  , trySolvers
  , NoSolvers(..)
  ) where

import Control.Exception (Exception(..), throwIO)
import Control.Monad.IO.Class
import Ersatz.Solution
import System.Exit (ExitCode(..))
import System.IO.Error (isDoesNotExistError, tryIOError, ioError)
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

-- | This error is thrown by 'trySolvers' when no solvers are found.
data NoSolvers = NoSolvers deriving Show

instance Exception NoSolvers where
  displayException _ = "no ersatz solvers were found"

-- | Try a list of solvers in order. When a solver fails due to
-- a missing executable the next solver will be tried. Throws
-- 'NoSolvers' exception if none of the given solvers were installed.
trySolvers :: [Solver s IO] -> Solver s IO
trySolvers [] _ = throwIO NoSolvers
trySolvers (solver:solvers) problem =
  do res <- tryIOError (solver problem)
     case res of
       Left e
         | isDoesNotExistError e -> trySolvers solvers problem
         | otherwise             -> ioError e
       Right x                   -> return x
