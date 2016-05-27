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
import Control.Applicative
import Control.Monad.IO.Class
import Data.IntMap (IntMap)
import Ersatz.Internal.Parser
import Ersatz.Problem
import Ersatz.Solution
import Ersatz.Solver.Common
import qualified Data.IntMap as IntMap
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
                    "sat":_   -> Satisfied
                    "unsat":_ -> Unsatisfied
                    _         -> Unsolved

    return (result, parseSolution out)

parseSolution :: String -> IntMap Bool
parseSolution input =
  case runParser solution input of
    s:_ -> s
    _   -> IntMap.empty

solution :: Parser Char (IntMap Bool)
solution = do
  _ <- string "sat\n"
  IntMap.fromList <$> values

values :: Parser Char [(Int, Bool)]
values  = many value <* token '\n' <* eof

value :: Parser Char (Int, Bool)
value = toPair <$> integer <* token ' '
  where
    toPair n | n >= 0    = ( n, True)
             | otherwise = (-n, False)
