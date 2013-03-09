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

class Variable t where
  exists :: MonadSAT m => m t
  forall :: MonadSAT m => m t

instance Variable Lit where
  exists = Lit <$> exists
  forall = Lit <$> forall

instance Variable Literal where
  exists = literalExists
  forall = literalForall

instance (Variable a, Variable b) => Variable (a,b) where
  exists = (,) <$> exists <*> exists
  forall = (,) <$> forall <*> forall

instance (Variable a, Variable b, Variable c) => Variable (a,b,c) where
  exists = (,,) <$> exists <*> exists <*> exists
  forall = (,,) <$> forall <*> forall <*> forall

instance (Variable a, Variable b, Variable c, Variable d) => Variable (a,b,c,d) where
  exists = (,,,) <$> exists <*> exists <*> exists <*> exists
  forall = (,,,) <$> forall <*> forall <*> forall <*> forall

instance (Variable a, Variable b, Variable c, Variable d, Variable e) => Variable (a,b,c,d,e) where
  exists = (,,,,) <$> exists <*> exists <*> exists <*> exists <*> exists
  forall = (,,,,) <$> forall <*> forall <*> forall <*> forall <*> forall

instance (Variable a, Variable b, Variable c, Variable d, Variable e, Variable f) => Variable (a,b,c,d,e,f) where
  exists = (,,,,,) <$> exists <*> exists <*> exists <*> exists <*> exists <*> exists
  forall = (,,,,,) <$> forall <*> forall <*> forall <*> forall <*> forall <*> forall

instance (Variable a, Variable b, Variable c, Variable d, Variable e, Variable f, Variable g) => Variable (a,b,c,d,e,f,g) where
  exists = (,,,,,,) <$> exists <*> exists <*> exists <*> exists <*> exists <*> exists <*> exists
  forall = (,,,,,,) <$> forall <*> forall <*> forall <*> forall <*> forall <*> forall <*> forall

instance (Variable a, Variable b, Variable c, Variable d, Variable e, Variable f, Variable g, Variable h) => Variable (a,b,c,d,e,f,g,h) where
  exists = (,,,,,,,) <$> exists <*> exists <*> exists <*> exists <*> exists <*> exists <*> exists <*> exists
  forall = (,,,,,,,) <$> forall <*> forall <*> forall <*> forall <*> forall <*> forall <*> forall <*> forall

