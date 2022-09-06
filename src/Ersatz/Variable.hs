{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------
-- |
-- Copyright :  © Edward Kmett 2010-2014, Johan Kiviniemi 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ersatz.Variable
  ( Variable(..)
#ifndef HLINT
  , forall
#endif
  , exists

  , GVariable(..)
  , genericLiterally
  ) where

import Control.Monad
import Ersatz.Internal.Literal
import Ersatz.Problem
import GHC.Generics

exists :: (Variable a, MonadSAT s m)  => m a
exists = literally literalExists

#ifndef HLINT
forall :: (Variable a, MonadQSAT s m)  => m a
forall = literally literalForall
#endif

class GVariable f where
  gliterally :: MonadSAT s m => m Literal -> m (f a)

instance GVariable U1 where
  gliterally _ = return U1

instance (GVariable f, GVariable g) => GVariable (f :*: g) where
  gliterally m = liftM2 (:*:) (gliterally m) (gliterally m)

instance Variable a => GVariable (K1 i a) where
  gliterally = fmap K1 . literally

instance GVariable f => GVariable (M1 i c f) where
  gliterally = fmap M1 . gliterally

-- | This class describes all types that can be represented
-- by a collection of literals. The class method 'literally'
-- is usually applied to 'literalExists' or 'literalForall',
-- see implementations of 'exists' and 'forall'.
--
-- Instances for this class for product-like types can be automatically derived
-- for any type that is an instance of 'Generic'.
--
-- === __Example usage__
--
-- @
-- {-# language DeriveGeneric, TypeApplications #-}
-- import GHC.Generics
--
-- data T = C Bit Bit deriving Generic
-- instance Variable T
--
-- constraint = do t <- exists @T ; ...
-- @

class Variable t where
  literally :: MonadSAT s m => m Literal -> m t
  default literally ::
    (MonadSAT s m, Generic t, GVariable (Rep t)) =>
    m Literal -> m t
  literally = genericLiterally

genericLiterally ::
  (MonadSAT s m, Generic t, GVariable (Rep t)) =>
  m Literal -> m t
genericLiterally = fmap to . gliterally

instance Variable Literal where
  literally = id

instance (Variable a, Variable b) => Variable (a,b)
instance (Variable a, Variable b, Variable c) => Variable (a,b,c)
instance (Variable a, Variable b, Variable c, Variable d) => Variable (a,b,c,d)
instance (Variable a, Variable b, Variable c, Variable d, Variable e) => Variable (a,b,c,d,e)
instance (Variable a, Variable b, Variable c, Variable d, Variable e, Variable f) => Variable (a,b,c,d,e,f)
instance (Variable a, Variable b, Variable c, Variable d, Variable e, Variable f, Variable g) => Variable (a,b,c,d,e,f,g)
