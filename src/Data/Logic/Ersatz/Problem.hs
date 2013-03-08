--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2010-2013, Johan Kiviniemi 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
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
