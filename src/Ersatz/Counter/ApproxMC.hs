{-# LANGUAGE ScopedTypeVariables #-}
module Ersatz.Counter.ApproxMC 
  ( approxmc
  , approxmcPath
  ) where

import Control.Monad.IO.Class
import Ersatz.Counter.Common (parseCounter)
import Ersatz.Solver.Common (withTempFiles)
import Ersatz.Problem (SAT, writeDimacs', writeProjectionSet)
import Ersatz.Solution (Counter)
import System.Process (readProcessWithExitCode)

-- | `Counter` for `SAT` problems that tries to invoke the @approxmc@ executable from the @PATH@
approxmc :: MonadIO m => Counter SAT m
approxmc = approxmcPath "approxmc"

-- | `Counter` for `SAT` problems that tries to invoke a program that takes @approxmc@ compatible arguments.
--
-- The 'FilePath' refers to the path to the executable.
approxmcPath :: MonadIO m => FilePath -> Counter SAT m
approxmcPath path problem projectionSet = liftIO $ do
  withTempFiles ".cnf" "" $ \problemPath _ -> do
    writeDimacs' problemPath problem
    writeProjectionSet problemPath projectionSet
    
    (_exit, out, _err) <-
      readProcessWithExitCode path [problemPath] []
    
    return $ parseCounter out