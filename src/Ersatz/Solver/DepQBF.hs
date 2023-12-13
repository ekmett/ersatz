--------------------------------------------------------------------
-- |
-- Copyright :  © Edward Kmett 2010-2014, Johan Kiviniemi 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- <http://lonsing.github.io/depqbf/ DepQBF> is a solver capable of
-- solving quantified boolean formulae ('QBF').
--------------------------------------------------------------------
module Ersatz.Solver.DepQBF
  ( depqbf
  , depqbfPath
  ) where

import Data.ByteString.Builder
import Control.Monad.IO.Class
import Ersatz.Problem ( QSAT, writeQdimacs' )
import Ersatz.Solution
import Ersatz.Solver.Common
import qualified Data.IntMap as I
import System.IO
import System.Process (readProcessWithExitCode)

-- | This is a 'Solver' for 'QSAT' problems that runs the @depqbf@ solver using
-- the current @PATH@, it tries to run an executable named @depqbf@.
depqbf :: MonadIO m => Solver QSAT m
depqbf = depqbfPath "depqbf"

parseLiteral :: String -> (Int, Bool)
parseLiteral ('-':xs) = (read xs, False)
parseLiteral xs = (read xs, True)

-- Parse the QDIMACS output format, which is described in
-- http://www.qbflib.org/qdimacs.html#output
parseOutput :: String -> [(Int, Bool)]
parseOutput out =
  case lines out of
    (_preamble:certLines) -> map parseCertLine certLines
    [] -> error "QDIMACS output without preamble"
  where
    parseCertLine :: String -> (Int, Bool)
    parseCertLine certLine =
      case words certLine of
        (_v:lit:_) -> parseLiteral lit
        _ -> error $ "Malformed QDIMACS certificate line: " ++ certLine

-- | This is a 'Solver' for 'QSAT' problems that lets you specify the path to the @depqbf@ executable.
depqbfPath :: MonadIO m => FilePath -> Solver QSAT m
depqbfPath path problem = liftIO $
  withTempFiles ".cnf" "" $ \problemPath _ -> do
    writeQdimacs' problemPath problem

    (exit, out, _err) <-
      readProcessWithExitCode path [problemPath, "--qdo"] []

    let result = resultOf exit

    return $ (,) result $
      case result of
        Satisfied ->
          I.fromList $ parseOutput out
        _ ->
          I.empty
