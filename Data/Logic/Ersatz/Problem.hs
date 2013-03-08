module Data.Logic.Ersatz.Problem
  ( QDIMACS(..)
  , Literal(literalId), negateLiteral
  , Lit, lit, negateLit
  , QBF(qbfLastAtom, qbfFormula, qbfUniversals), emptyQBF
  , Formula, Clause, clauseLiterals
  , MonadSAT(..)
  , SAT, satToIO
  , Variable(..)
  ) where

import Data.Logic.Ersatz.Internal.Problem
