module Data.Logic.Ersatz.Problem
    ( QBF(qbfLastAtom, qbfClauses, qbfUniversals), emptyQBF
    , Clause, clauseLiterals
    , Clauses
    , Literal(literalId), negateLiteral -- literalForall, literalExists
    , Lit, lit, negateLit, litForall, litExists
    , QDIMACS(..)
    , Variable(..)
    , assertLits
    -- , assume
    -- , reifyLit
    ) where

import Data.Logic.Ersatz.Internal.Problem
