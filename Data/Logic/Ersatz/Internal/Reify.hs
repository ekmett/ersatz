{-# LANGUAGE TypeFamilies, Rank2Types #-}
module Data.Logic.Ersatz.Internal.Reify 
    ( MuRef(..)
    , DynStableName(..)
    , hashDynStableName
    , makeDynStableName
    , DynStableMap 
    , insertDynStableMap
    , lookupDynStableMap
    ) where

import Control.Applicative (Applicative, (<$>))
-- import System.IO.Unsafe (unsafePerformIO)
import System.Mem.StableName (StableName, makeStableName, hashStableName)
import Data.IntMap as IntMap
import Data.IntMap (IntMap)
import Unsafe.Coerce (unsafeCoerce)
import Control.Monad.ST

class MuRef s where
    type DeRef (s :: * -> *) :: (* -> *) -> * -> *
    mapDeRef :: Applicative m => 
                (forall t. (MuRef t, DeRef s ~ DeRef t) => t b-> m (u b)) -> 
                s b -> m (DeRef s u b)

newtype DynStableName = DynStableName (StableName ())

hashDynStableName :: DynStableName -> Int
hashDynStableName (DynStableName sn) = hashStableName sn

instance Eq DynStableName where
    DynStableName sn1 == DynStableName sn2 = sn1 == sn2

makeDynStableName :: t -> ST b DynStableName
makeDynStableName a = DynStableName . unsafeCoerce <$> unsafeIOToST (makeStableName a)

type DynStableMap t = IntMap [(DynStableName, t)]

insertDynStableMap :: DynStableName -> t -> DynStableMap t -> DynStableMap t 
insertDynStableMap k v = IntMap.insertWith (++) (hashDynStableName k) [(k,v)]

lookupDynStableMap :: DynStableName -> DynStableMap t -> Maybe t
lookupDynStableMap k m = do
    pairs <- IntMap.lookup (hashDynStableName k) m
    Prelude.lookup k pairs
