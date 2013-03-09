--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2010-2013, Johan Kiviniemi 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ersatz.Problem
  (
  -- * SAT
    SAT
  , MonadSAT(..)
  , unsat
  , Variable(..)
  -- * Implementation Details
  -- ** QDIMACS encoding
  , QDIMACS(..)
  -- ** Quantified Boolean Formulas
  , QBF(qbfLastAtom, qbfFormula, qbfUniversals), emptyQBF
  , Formula, Clause, clauseLiterals
  -- ** Literals
  , Literal(literalId), negateLiteral
  , Lit, lit, negateLit
  ) where

import Ersatz.Internal.Problem
