{-# LANGUAGE Safe #-}
--------------------------------------------------------------------
-- |
-- Copyright :  © Edward Kmett 2010-2014, Johan Kiviniemi 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
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
import qualified Data.IntMap as IntMap
import System.IO
import System.Process (readProcessWithExitCode)

depqbf :: MonadIO m => Solver QSAT m
depqbf = depqbfPath "depqbf"

depqbfPath :: MonadIO m => FilePath -> Solver QSAT m
depqbfPath path problem = liftIO $
  withTempFiles ".cnf" "" $ \problemPath _ -> do
    withFile problemPath WriteMode $ \fh ->
      hPutBuilder fh (qdimacs problem)

    (exit, _out, _err) <-
      readProcessWithExitCode path [problemPath] []

    return (resultOf exit, IntMap.empty)
