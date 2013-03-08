--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2010-2013, Johan Kiviniemi 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.Logic.Ersatz.Internal.StableName
  ( StableName
  , makeStableName'
  ) where

import System.Mem.StableName (StableName, makeStableName)
import Unsafe.Coerce (unsafeCoerce)

makeStableName' :: a -> IO (StableName ())
makeStableName' a = a `seq` fmap coerceStableName (makeStableName a)
  where
    coerceStableName :: StableName a -> StableName ()
    coerceStableName = unsafeCoerce
