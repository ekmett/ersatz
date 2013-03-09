{-# LANGUAGE DeriveDataTypeable #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2010-2013, Johan Kiviniemi 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ersatz.Internal.Literal
  ( Literal(..)
  , negateLiteral
  , Lit(..)
  , lit
  , negateLit
  ) where

import Data.Typeable

-- | A naked possibly-negated Atom, present in the target 'Ersatz.Solver.Solver'.
newtype Literal = Literal { literalId :: Int } deriving (Eq,Ord,Typeable)

instance Show Literal where
  showsPrec i = showsPrec i . literalId
  show = show . literalId
  showList = showList . map literalId

negateLiteral :: Literal -> Literal
negateLiteral = Literal . negate . literalId

-- | Literals with partial evaluation
data Lit
  = Lit  {-# UNPACK #-} !Literal
  | Bool !Bool
  deriving (Show, Typeable)

-- | Lift a 'Bool' to a 'Lit'
lit :: Bool -> Lit
lit = Bool

negateLit :: Lit -> Lit
negateLit (Bool b) = Bool (not b)
negateLit (Lit l) = Lit (negateLiteral l)
