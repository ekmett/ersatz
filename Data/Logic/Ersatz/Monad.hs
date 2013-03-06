{-# LANGUAGE Rank2Types #-}

module Data.Logic.Ersatz.Monad
  ( SAT
  , MonadSAT(..)
  , solveWith
  , satToIO
  , showSAT
  ) where

import Control.Applicative
import Control.Monad.State (runStateT)
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.IntMap as IntMap

import Data.Logic.Ersatz.Encoding
import Data.Logic.Ersatz.Internal.Problem
import Data.Logic.Ersatz.Solution

solveWith :: Encoding a => Solver IO -> SAT a -> IO (Result, Maybe (Decoded a))
solveWith solver sat = do
  (a, qbf) <- satToIO sat
  (res, litMap) <- solver qbf
  let snMap = HashMap.map (\v -> let v' = literalId v
                                 in  if v' >= 0 then litMap IntMap.! v'
                                                else not (litMap IntMap.! (-v')))
            . HashMap.filter (\v -> IntMap.member (abs (literalId v)) litMap)
            $ qbfSNMap qbf
      sol = Solution litMap snMap
  (,) res <$> decode sol a

showSAT :: SAT a -> IO String
showSAT = fmap (show . snd) . satToIO

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
