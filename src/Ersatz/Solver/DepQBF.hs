{-# LANGUAGE Trustworthy #-}
--------------------------------------------------------------------
-- |
-- Copyright :  © Edward Kmett 2010-2013, Johan Kiviniemi 2013
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

import Blaze.ByteString.Builder -- not Trustworthy
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
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
      toByteStringIO (BS.hPut fh) (qdimacs problem)

    (exit, _out, _err) <-
      readProcessWithExitCode path [problemPath] []

    return (resultOf exit, IntMap.empty)
