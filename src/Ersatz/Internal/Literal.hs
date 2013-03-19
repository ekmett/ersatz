{-# LANGUAGE DeriveDataTypeable #-}
--------------------------------------------------------------------
-- |
-- Copyright :  © Edward Kmett 2010-2013, Johan Kiviniemi 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ersatz.Internal.Literal
  ( Literal(..)
  , negateLiteral
  , literalFalse, literalTrue
  ) where

import Data.Typeable

-- | A naked possibly-negated Atom, present in the target 'Ersatz.Solver.Solver'.
--
-- The literals @-1@ and @1@ are dedicated for the constant 'False' and the
-- constant 'True' respectively.
newtype Literal = Literal { literalId :: Int } deriving (Eq,Ord,Typeable)

instance Show Literal where
  showsPrec i = showsPrec i . literalId
  show = show . literalId
  showList = showList . map literalId

negateLiteral :: Literal -> Literal
negateLiteral = Literal . negate . literalId

-- | The 'False' constant. The literal @-1@ is dedicated for it.
literalFalse :: Literal
literalFalse = Literal (-1)

-- | The 'True' constant. The literal @1@ is dedicated for it.
literalTrue :: Literal
literalTrue = Literal 1
