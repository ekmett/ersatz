{-# LANGUAGE Trustworthy #-}
--------------------------------------------------------------------
-- |
-- Copyright :  © Edward Kmett 2010-2014, Johan Kiviniemi 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
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

minisat :: MonadIO m => Solver SAT m
minisat = minisatPath "minisat"

cryptominisat :: MonadIO m => Solver SAT m
cryptominisat = minisatPath "cryptominisat"

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
parseSolutionFile path = handle handler (parseSolution <$> readFile path)
  where
    handler :: IOException -> IO (IntMap Bool)
    handler _ = return IntMap.empty

parseSolution :: String -> IntMap Bool
parseSolution input =
  case runParser solution input of
    s:_ -> s
    _   -> IntMap.empty

solution :: Parser Char (IntMap Bool)
solution = do
  _ <- string "SAT\n"
  IntMap.fromList <$> values

values :: Parser Char [(Int, Bool)]
values  = (value `sepBy` token ' ')
       <* string " 0"
       <* (() <$ token '\n' <|> eof)

value :: Parser Char (Int, Bool)
value = do
  i <- integer
  guard (i /= 0)
  return (toPair i)
  where
    toPair n | n >= 0    = ( n, True)
             | otherwise = (-n, False)
