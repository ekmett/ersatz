--------------------------------------------------------------------
-- |
-- Copyright :  © Edward Kmett 2010-2014, Johan Kiviniemi 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

{-# language OverloadedStrings #-}

module Ersatz.Solver.Minisat
  ( minisat
  , cryptominisat
  , minisatPath
  , cryptominisat5
  , cryptominisat5Path
  , anyminisat
  ) where

import Data.ByteString.Builder
import Control.Exception (IOException, handle)
import Control.Monad.IO.Class
import Data.IntMap (IntMap)
import Ersatz.Problem ( SAT, writeDimacs' )
import Ersatz.Solution
import Ersatz.Solver.Common
import qualified Data.IntMap.Strict as IntMap
import System.IO
import System.Process (readProcessWithExitCode)

import qualified Data.ByteString.Char8 as B
import qualified Data.List as List ( foldl' )

-- | Hybrid 'Solver' that tries to use: 'cryptominisat5', 'cryptominisat', and 'minisat'
anyminisat :: Solver SAT IO
anyminisat = trySolvers [cryptominisat5, cryptominisat, minisat]

-- | 'Solver' for 'SAT' problems that tries to invoke the @minisat@ executable from the @PATH@
minisat :: MonadIO m => Solver SAT m
minisat = minisatPath "minisat"

-- | 'Solver' for 'SAT' problems that tries to invoke the @cryptominisat@ executable from the @PATH@
cryptominisat :: MonadIO m => Solver SAT m
cryptominisat = minisatPath "cryptominisat"

-- | 'Solver' for 'SAT' problems that tries to invoke a program that takes @minisat@ compatible arguments.
--
-- The 'FilePath' refers to the path to the executable.
minisatPath :: MonadIO m => FilePath -> Solver SAT m
minisatPath path problem = liftIO $
  withTempFiles ".cnf" "" $ \problemPath solutionPath -> do
    writeDimacs' problemPath problem

    (exit, _out, _err) <-
      readProcessWithExitCode path [problemPath, solutionPath] []

    sol <- parseSolutionFile solutionPath

    return (resultOf exit, sol)

parseSolutionFile :: FilePath -> IO (IntMap Bool)
parseSolutionFile path = handle handler (parseSolution <$> B.readFile path)
  where
    handler :: IOException -> IO (IntMap Bool)
    handler _ = return IntMap.empty

parseSolution :: B.ByteString -> IntMap Bool
parseSolution s =
  case B.words s of
    x : ys | x == "SAT" ->
          List.foldl'
                 ( \ m y -> case B.readInt y of
                              Just (v,_) -> if 0 == v then m else IntMap.insert (abs v) (v>0) m
                              Nothing    -> error $ "parseSolution: Expected an Int, received " ++ show y
                 ) IntMap.empty ys
    _ -> IntMap.empty -- WRONG (should be Nothing)

-- | 'Solver' for 'SAT' problems that tries to invoke the @cryptominisat5@ executable from the @PATH@
cryptominisat5 :: MonadIO m => Solver SAT m
cryptominisat5 = cryptominisat5Path "cryptominisat5"

-- | 'Solver' for 'SAT' problems that tries to invoke a program that takes @cryptominisat5@ compatible arguments.
--
-- The 'FilePath' refers to the path to the executable.
cryptominisat5Path :: MonadIO m => FilePath -> Solver SAT m
cryptominisat5Path path problem = liftIO $
  withTempFiles ".cnf" "" $ \problemPath _ -> do
    writeDimacs' problemPath problem

    (exit, out, _err) <-
      readProcessWithExitCode path [problemPath] []

    let sol = parseSolution5 out

    return (resultOf exit, sol)
