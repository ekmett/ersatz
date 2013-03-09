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
  = Lit  { getLiteral  :: {-# UNPACK #-} !Literal }
  | Bool { getValue :: !Bool }
  deriving Typeable

instance Show Lit where
  showsPrec p (Lit l)  = showParen (p > 10)
                       $ showString "Lit " . showsPrec 11 l
  showsPrec p (Bool b) = showParen (p > 10)
                       $ showString "Bool " . showsPrec 11 b

-- | Lift a 'Bool' to a 'Lit'
lit :: Bool -> Lit
lit = Bool

negateLit :: Lit -> Lit
negateLit (Bool b) = Bool (not b)
negateLit (Lit l) = Lit (negateLiteral l)
