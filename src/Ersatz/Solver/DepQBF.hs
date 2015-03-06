{-# LANGUAGE Safe #-}
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
import Ersatz.Problem
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

-- | This is a 'Solver' for 'QSAT' problems that lets you specify the path to the @depqbf@ executable.
depqbfPath :: MonadIO m => FilePath -> Solver QSAT m
depqbfPath path problem = liftIO $
  withTempFiles ".cnf" "" $ \problemPath _ -> do
    withFile problemPath WriteMode $ \fh ->
      hPutBuilder fh (qdimacs problem)

    (exit, out, _err) <-
      readProcessWithExitCode path [problemPath, "--qdo"] []

    let result = resultOf exit

    return $ (,) result $
      case result of
        Satisfied ->
          I.fromList $ map (parseLiteral . head . tail . words) $ tail $ lines out
        _ ->
          I.empty
