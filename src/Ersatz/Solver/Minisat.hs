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
  ) where

import Data.ByteString.Builder
import Control.Applicative
import Control.Exception (IOException, handle)
import Control.Monad
import Control.Monad.IO.Class
import Data.IntMap (IntMap)
import Ersatz.Internal.Parser
import Ersatz.Problem
import Ersatz.Solution
import Ersatz.Solver.Common
import qualified Data.IntMap as IntMap
import System.IO
import System.Process (readProcessWithExitCode)

import qualified Data.Attoparsec.ByteString.Char8 as  P
import qualified Data.ByteString as B

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
    withFile problemPath WriteMode $ \fh ->
      hPutBuilder fh (dimacs problem)

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
parseSolution s = case P.parseOnly ( assignment <* P.endOfInput ) s of
    Left err -> IntMap.empty -- WRONG
    Right ps -> IntMap.fromList ps

assignment :: P.Parser [(Int,Bool)]
assignment = P.string "SAT" *> P.skipSpace
   *> many ( do v <- P.signed P.decimal; guard $ v /= 0 ; P.skipSpace ; return (abs v, v > 0) )
   <* P.string "0" <* P.skipSpace

