{-# LANGUAGE Rank2Types #-}

module Data.Logic.Ersatz.Monad
    ( SAT
    , MonadSAT(..)
    , satToIO
    , showSAT
    ) where

import Control.Monad.State (runStateT)
-- import Control.Monad.Trans (MonadIO(..))
import Data.Logic.Ersatz.Internal.Problem
import System.IO.Unsafe
-- import Data.Logic.Ersatz.Solution

showSAT :: SAT a -> String    
showSAT = qdimacs . snd . unsafePerformIO . satToIO

satToIO :: SAT a -> IO (a, QBF)
satToIO m = runStateT (runSAT m) emptyQBF

{-
withST :: (forall s. (SAT s a, a -> QBF s -> ST s r)) -> r
withST (m, k) = runST (do (a, qbf) <- satToST m; k a qbf)

withIO  :: (forall s. (SAT s a, a -> QBF s -> IO r)) -> IO r
withIO (m, k) = do (a, qbf) <- stToIO (satToST m)
                   k a qbf

solveWith :: MonadIO m => Solver m -> (forall s. (SAT s a, Witness s a -> m r)) -> m r
solveWith solve (m, k) = do
    (a, qbf) <- liftIO $ stToIO (satToST m)
    (result, certificate) <- solve qbf
    k (Witness result certificate a)
-}
