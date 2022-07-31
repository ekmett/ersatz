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

  , parseSolution5
  ) where

import Control.Exception (Exception(..), throwIO)
import Control.Monad.IO.Class
import Ersatz.Solution
import System.Exit (ExitCode(..))
import System.IO.Error (isDoesNotExistError, tryIOError)
import System.IO.Temp (withSystemTempDirectory)
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap

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
newtype NoSolvers = NoSolvers [IOError] deriving Show

instance Exception NoSolvers where
  displayException _ = "no ersatz solvers were found"

-- | Try a list of solvers in order. When a solver fails due to
-- a missing executable the next solver will be tried. Throws
-- 'NoSolvers' exception if none of the given solvers were installed.
trySolvers :: [Solver s IO] -> Solver s IO
trySolvers solvers problem = foldr runSolver noSolvers solvers []
  where
    noSolvers = throwIO . NoSolvers . reverse

    runSolver solver next es =
      do res <- tryIOError (solver problem)
         case res of
           Left  e -> next (e:es)
           Right x -> return x

parseSolution5 :: String -> IntMap Bool
parseSolution5 txt = IntMap.fromList [(abs v, v > 0) | v <- vars, v /= 0]
  where
    vlines = [l | ('v':l) <- lines txt]
    vars = map read (foldMap words vlines)
