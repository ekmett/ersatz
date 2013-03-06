module Data.Logic.Ersatz.Solver.Minisat
  ( minisat
  , cryptominisat
  , minisatPath
  ) where

import Control.Applicative
import Control.Exception (IOException, handle)
import Control.Monad
import qualified Data.IntMap as IntMap
import System.Exit (ExitCode(..))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)

import Data.Logic.Ersatz.Internal.Problem (qdimacs)
import Data.Logic.Ersatz.Internal.TrivialParser
import Data.Logic.Ersatz.Solution

minisat :: Solver IO
minisat = minisatPath "minisat"

cryptominisat :: Solver IO
cryptominisat = minisatPath "cryptominisat"

minisatPath :: FilePath -> Solver IO
minisatPath path qbf =
  withSystemTempDirectory "ersatz" $ \dir -> do
    let problemPath  = dir ++ "/problem.cnf"
        solutionPath = dir ++ "/solution"

    writeFile problemPath (qdimacs qbf)

    (exit, _out, _err) <-
      readProcessWithExitCode path [problemPath, solutionPath] []

    sol <- parseSolutionFile solutionPath

    return (resultOf exit, sol)

resultOf :: ExitCode -> Result
resultOf (ExitFailure 10) = Satisfied
resultOf (ExitFailure 20) = Unsatisfied
resultOf _                = Unsolved

parseSolutionFile :: FilePath -> IO Solution
parseSolutionFile path = handle handler $ do
  parseSolution <$> readFile path
  where
    handler :: IOException -> IO Solution
    handler _ = return IntMap.empty

parseSolution :: String -> Solution
parseSolution input =
  case runParser solution input of
    (s:_) -> s
    _     -> IntMap.empty

solution :: Parser Char Solution
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
