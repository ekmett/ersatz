{-# LANGUAGE TypeFamilies, Rank2Types #-}
-- a mishmash of features taken from Data.Reify
module Data.Logic.Ersatz.Internal.Reify
  ( MuRef(..)
  , DynStableName(..)
  , hashDynStableName
  , makeDynStableName
  , DynStableMap
  , insertDynStableMap
  , lookupDynStableMap
  ) where

import Control.Applicative ((<$>))
import System.Mem.StableName (StableName, makeStableName, hashStableName)
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
-- import Data.Unique
import Data.Reify (MuRef(..))
import Unsafe.Coerce (unsafeCoerce)

newtype DynStableName = DynStableName (StableName ())

hashDynStableName :: DynStableName -> Int
hashDynStableName (DynStableName sn) = hashStableName sn

instance Eq DynStableName where
  DynStableName sn1 == DynStableName sn2 = sn1 == sn2

makeDynStableName :: t -> IO DynStableName
makeDynStableName a = DynStableName . unsafeCoerce <$> makeStableName a

type DynStableMap t = IntMap [(DynStableName, t)]

insertDynStableMap :: DynStableName -> t -> DynStableMap t -> DynStableMap t
insertDynStableMap k v = IntMap.insertWith (++) (hashDynStableName k) [(k,v)]

lookupDynStableMap :: DynStableName -> DynStableMap t -> Maybe t
lookupDynStableMap k m = do
  pairs <- IntMap.lookup (hashDynStableName k) m
  Prelude.lookup k pairs

{-
data Graph e = Graph [(Unique,e Unique)] Unique

-- | If 'e' is s Functor, and 'e' is 'Show'-able, then we can 'Show' a 'Graph'.
instance (Show (e Int)) => Show (Graph e) where
  show (Graph netlist start) = unwords ["let", show netlist,"in", show start]
-}


{-
-- | 'reifyGraph' takes a data structure that admits 'MuRef', and returns a 'Graph' that contains
-- the dereferenced nodes, with their children as 'Unique' rather than recursive values.
reifyGraph :: (MuRef s) => s -> IO (Graph (DeRef s))
reifyGraph m = do rt1 <- newMVar M.empty
                  rt2 <- newMVar []
                  root <- findNodes rt1 rt2 m
                  pairs <- readMVar rt2
                  return (Graph pairs root)


findNodes :: (MuRef s)
          => MVar (IntMap [(StableName s,Unique)])   -- Dynamic of StableNames
          -> MVar [(Unique,DeRef s Unique)]
          -> s
          -> IO Unique
findNodes rt1 rt2 j | j `seq` True = do
  st <- makeDynStableName j
  tab <- takeMVar rt1
  case mylookup st tab of
    Just var -> do
      putMVar rt1 tab
      return $ var
    Nothing -> do
      var <- newUnique
      putMVar rt1 $ M.insertWith (++) (hashDynStableName st) [(st,var)] tab
      res <- mapDeRef (findNodes rt1 rt2) j
      tab' <- takeMVar rt2
      putMVar rt2 $ (var,res) : tab'
      return var
  where
    mylookup h tab =
      case M.lookup (hashStableName h) tab of
        Just tab2 -> Prelude.lookup h tab2
        Nothing ->  Nothing
-}
