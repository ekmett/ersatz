module Data.Logic.Ersatz.Problem
  ( QBF(qbfLastAtom, qbfFormula, qbfUniversals), emptyQBF
  , Formula, Clause, clauseLiterals
  , Literal(literalId), negateLiteral -- literalForall, literalExists
  , Lit, lit, negateLit, litForall, litExists
  , QDIMACS(..)
  , Variable(..)
  -- , assertLits
  -- , assume
  -- , reifyLit
  ) where

import Data.Logic.Ersatz.Internal.Problem
