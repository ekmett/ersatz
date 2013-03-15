{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2010-2013, Johan Kiviniemi 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ersatz.Variable
  ( Variable(..)
  ) where

import Control.Monad
import Control.Monad.State.Class
import Ersatz.Internal.Literal
import Ersatz.Problem
import GHC.Generics

class GVariable f where
  gexists :: (MonadState s m, HasSAT s)  => m (f a)
  gforall :: (MonadState s m, HasQSAT s) => m (f a)

instance GVariable U1 where
  gexists = return U1
  gforall = return U1

instance (GVariable f, GVariable g) => GVariable (f :*: g) where
  gexists = liftM2 (:*:) gexists gexists
  gforall = liftM2 (:*:) gforall gforall

instance Variable a => GVariable (K1 i a) where
  gexists = liftM K1 exists
  gforall = liftM K1 forall

instance GVariable f => GVariable (M1 i c f) where
  gexists = liftM M1 gexists
  gforall = liftM M1 gforall

-- | Instances for this class for product-like types can be automatically derived
-- for any type that is an instance of 'Generic'.
class Variable t where
  exists :: (MonadState s m, HasSAT s) => m t
  forall :: (MonadState s m, HasQSAT s) => m t

#ifndef HLINT
  default exists :: (MonadState s m, HasSAT s, Generic t, GVariable (Rep t)) => m t
  exists = liftM to gexists

  default forall :: (MonadState s m, HasQSAT s, Generic t, GVariable (Rep t)) => m t
  forall = liftM to gforall
#endif

instance Variable Literal where
  exists = literalExists
  forall = literalForall

instance (Variable a, Variable b) => Variable (a,b)
instance (Variable a, Variable b, Variable c) => Variable (a,b,c)
instance (Variable a, Variable b, Variable c, Variable d) => Variable (a,b,c,d)
instance (Variable a, Variable b, Variable c, Variable d, Variable e) => Variable (a,b,c,d,e)
instance (Variable a, Variable b, Variable c, Variable d, Variable e, Variable f) => Variable (a,b,c,d,e,f)
instance (Variable a, Variable b, Variable c, Variable d, Variable e, Variable f, Variable g) => Variable (a,b,c,d,e,f,g)
