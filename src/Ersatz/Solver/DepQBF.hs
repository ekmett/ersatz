--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2010-2013, Johan Kiviniemi 2013
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

import Control.Monad.IO.Class
import Ersatz.Problem (qdimacs)
import Ersatz.Solution
import Ersatz.Solver.Common
import qualified Data.IntMap as IntMap
import System.Process (readProcessWithExitCode)

depqbf :: MonadIO m => Solver m
depqbf = depqbfPath "depqbf"

depqbfPath :: MonadIO m => FilePath -> Solver m
depqbfPath path qbf = liftIO $
  withTempFiles ".cnf" "" $ \problemPath _ -> do
    writeFile problemPath (qdimacs qbf)

    (exit, _out, _err) <-
      readProcessWithExitCode path [problemPath] []

    return (resultOf exit, IntMap.empty)
