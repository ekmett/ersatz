{-# LANGUAGE DefaultSignatures, TypeOperators, FlexibleContexts, UndecidableInstances #-}
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

import Control.Applicative
import Ersatz.Internal.Literal
import Ersatz.Monad
import GHC.Generics

class GVariable f where
  gexists :: MonadSAT m => m (f a)
  gforall :: MonadSAT m => m (f a)

instance GVariable U1 where
  gexists = return U1
  gforall = return U1

instance (GVariable f, GVariable g) => GVariable (f :*: g) where
  gexists = (:*:) <$> gexists <*> gexists
  gforall = (:*:) <$> gforall <*> gforall

instance Variable a => GVariable (K1 i a) where
  gexists = K1 <$> exists
  gforall = K1 <$> forall

instance GVariable f => GVariable (M1 i c f) where
  gexists = M1 <$> gexists
  gforall = M1 <$> gforall

class Variable t where
  exists :: MonadSAT m => m t
  default exists :: (MonadSAT m, Generic t, GVariable (Rep t)) => m t
  exists = to <$> gexists

  forall :: MonadSAT m => m t
  default forall :: (MonadSAT m, Generic t, GVariable (Rep t)) => m t
  forall = to <$> gforall

instance Variable Lit where
  exists = Lit <$> exists
  forall = Lit <$> forall

instance Variable Literal where
  exists = literalExists
  forall = literalForall

instance (Variable a, Variable b) => Variable (a,b)
instance (Variable a, Variable b, Variable c) => Variable (a,b,c)
instance (Variable a, Variable b, Variable c, Variable d) => Variable (a,b,c,d)
instance (Variable a, Variable b, Variable c, Variable d, Variable e) => Variable (a,b,c,d,e)
instance (Variable a, Variable b, Variable c, Variable d, Variable e, Variable f) => Variable (a,b,c,d,e,f)
instance (Variable a, Variable b, Variable c, Variable d, Variable e, Variable f, Variable g) => Variable (a,b,c,d,e,f,g)
