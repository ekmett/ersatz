{-# LANGUAGE Rank2Types, GeneralizedNewtypeDeriving, TypeOperators, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, PatternGuards, DeriveDataTypeable, DefaultSignatures, TypeFamilies, BangPatterns, TemplateHaskell, OverloadedStrings #-}
{-# OPTIONS_HADDOCK not-home #-}
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
    SAT(SAT)
  , HasSAT(..)
  , runSAT, runSAT', dimacsSAT
  , literalExists
  , assertFormula
  , generateLiteral
  -- * QSAT
  , QSAT(QSAT)
  , HasQSAT(..)
  , runQSAT, runQSAT', qdimacsQSAT
  , literalForall
  -- * DIMACS pretty printing
  , DIMACS(..)
  , QDIMACS(..)
  , WDIMACS(..)
  , dimacs, qdimacs, wdimacs
  ) where

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char8
import Blaze.Text
import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Lazy
import Data.Default
import Data.Foldable (foldMap)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.Int
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.List as List
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable
import Ersatz.Internal.Formula
import Ersatz.Internal.Literal
import Ersatz.Internal.StableName
import System.IO.Unsafe

------------------------------------------------------------------------------
-- SAT Problems
------------------------------------------------------------------------------

data SAT = SAT
  { _lastAtom  :: {-# UNPACK #-} !Int      -- ^ The id of the last atom allocated
  , _formula   :: !Formula                 -- ^ a set of clauses to assert
  , _stableMap :: !(HashMap (StableName ()) Literal)  -- ^ a mapping used during 'Bit' expansion
  } deriving Typeable

makeLensesWith ?? ''SAT $ classyRules & lensClass.mapped ?~ ("HasSAT","sat")


instance Show SAT where
  showsPrec p bf = showParen (p > 10)
                 $ showString "SAT " . showsPrec 11 (bf^.lastAtom)
                 . showChar ' ' . showsPrec 11 (bf^.formula)
                 . showString " mempty"


instance Default SAT where
  def = SAT 0 (Formula Set.empty) HashMap.empty

-- | Run a 'SAT'-generating state computation. Useful e.g. in ghci for
-- disambiguating the type of a 'MonadState', 'HasSAT' value.
runSAT :: StateT SAT m a -> m (a, SAT)
runSAT s = runStateT s def

-- | Run a 'SAT'-generating state computation in the 'Identity' monad. Useful
-- e.g. in ghci for disambiguating the type of a 'MonadState', 'HasSAT' value.
runSAT' :: StateT SAT Identity a -> (a, SAT)
runSAT' = runIdentity . runSAT

-- | Run a 'SAT'-generating state computation and return the respective
-- 'DIMACS' output. Useful for testing and debugging.
dimacsSAT :: StateT SAT Identity a -> Lazy.ByteString
dimacsSAT = toLazyByteString . dimacs . snd . runSAT'

literalExists :: (MonadState s m, HasSAT s) => m Literal
literalExists = liftM Literal $ lastAtom <+= 1
{-# INLINE literalExists #-}

assertFormula :: (MonadState s m, HasSAT s) => Formula -> m ()
assertFormula xs = formula <>= xs
{-# INLINE assertFormula #-}

generateLiteral :: (MonadState s m, HasSAT s) => a -> (Literal -> m ()) -> m Literal
generateLiteral a f = do
  let sn = unsafePerformIO (makeStableName' a)
  use (stableMap.at sn) >>= \ ml -> case ml of
    Just l -> return l
    Nothing -> do
      l <- literalExists
      stableMap.at sn ?= l
      f l
      return l
{-# INLINE generateLiteral #-}

------------------------------------------------------------------------------
-- QSAT Problems
------------------------------------------------------------------------------

-- | A (quantified) boolean formula.
data QSAT = QSAT
  { _universals :: !IntSet -- ^ a set indicating which literals are universally quantified
  , _qsatSat    :: SAT     -- ^ The rest of the information, in 'SAT'
  } deriving (Show,Typeable)

class HasSAT t => HasQSAT t where
  qsat       :: Lens' t QSAT
  universals :: Lens' t IntSet
  universals f = qsat ago where
    ago (QSAT u s) = f u <&> \u' -> QSAT u' s

instance HasSAT QSAT where
  sat f (QSAT u s) = QSAT u <$> f s

instance HasQSAT QSAT where
  qsat = id

instance Default QSAT where
  def = QSAT IntSet.empty def

-- | Run a 'QSAT'-generating state computation. Useful e.g. in ghci for
-- disambiguating the type of a 'MonadState', 'HasQSAT' value.
runQSAT :: StateT QSAT m a -> m (a, QSAT)
runQSAT s = runStateT s def

-- | Run a 'QSAT'-generating state computation in the 'Identity' monad. Useful
-- e.g. in ghci for disambiguating the type of a 'MonadState', 'HasQSAT' value.
runQSAT' :: StateT QSAT Identity a -> (a, QSAT)
runQSAT' = runIdentity . runQSAT

-- | Run a 'QSAT'-generating state computation and return the respective
-- 'QDIMACS' output. Useful for testing and debugging.
qdimacsQSAT :: StateT QSAT Identity a -> Lazy.ByteString
qdimacsQSAT = toLazyByteString . qdimacs . snd . runQSAT'

literalForall :: (MonadState s m, HasQSAT s) => m Literal
literalForall = do
   l <- lastAtom <+= 1
   universals.contains l .= True
   return $ Literal l
{-# INLINE literalForall #-}

------------------------------------------------------------------------------
-- Printing SATs
------------------------------------------------------------------------------

-- | DIMACS file format pretty printer
--
-- This is used to generate the problem statement for a given 'SAT' 'Ersatz.Solver.Solver'.

-- http://www.cfdvs.iitb.ac.in/download/Docs/verification/papers/BMC/JT.ps
class DIMACS t where
  dimacsComments :: t -> [ByteString]
  dimacsNumVariables :: t -> Int
  dimacsClauses :: t -> Set IntSet

-- | QDIMACS file format pretty printer
--
-- This is used to generate the problem statement for a given 'QSAT' 'Ersatz.Solver.Solver'.

-- http://www.qbflib.org/qdimacs.html
class QDIMACS t where
  qdimacsComments :: t -> [ByteString]
  qdimacsNumVariables :: t -> Int
  qdimacsQuantified :: t -> [Quant]
  qdimacsClauses :: t -> Set IntSet

-- | WDIMACS file format pretty printer
--
-- This is used to generate the problem statement for a given 'MaxSAT' 'Ersatz.Solver.Solver' (TODO).

-- http://maxsat.ia.udl.cat/requirements/
class WDIMACS t where
  wdimacsComments :: t -> [ByteString]
  wdimacsNumVariables :: t -> Int
  wdimacsTopWeight :: t -> Int64  -- ^ Specified to be 1 ≤ n < 2^63
  wdimacsClauses :: t -> Set (Int64, IntSet)

-- | Generate a 'Builder' out of a 'DIMACS' problem.
dimacs :: DIMACS t => t -> Builder
dimacs t = comments <> problem <> clauses
  where
    comments = foldMap bComment (dimacsComments t)
    problem = bLine [ copyByteString "p cnf"
                    , integral (dimacsNumVariables t)
                    , integral (Set.size tClauses)
                    ]
    clauses = foldMap bClause tClauses

    tClauses = dimacsClauses t

-- | Generate a 'Builder' out of a 'QDIMACS' problem.
qdimacs :: QDIMACS t => t -> Builder
qdimacs t = comments <> problem <> quantified <> clauses
  where
    comments = foldMap bComment (qdimacsComments t)
    problem = bLine [ copyByteString "p cnf"
                    , integral (qdimacsNumVariables t)
                    , integral (Set.size tClauses)
                    ]
    -- TODO: "The innermost quantified set is always of type 'e'" per QDIMACS
    -- standard
    quantified = foldMap go tQuantGroups
      where go ls = bLine0 (q (head ls) : map (integral . getQuant) ls)
            q Exists{} = fromChar 'e'
            q Forall{} = fromChar 'a'
    clauses = foldMap bClause tClauses

    tQuantGroups = List.groupBy eqQuant (qdimacsQuantified t)
      where
        eqQuant :: Quant -> Quant -> Bool
        eqQuant Exists{} Exists{} = True
        eqQuant Forall{} Forall{} = True
        eqQuant _ _ = False
    tClauses = qdimacsClauses t

-- | Generate a 'Builder' out of a 'WDIMACS' problem.
wdimacs :: WDIMACS t => t -> Builder
wdimacs t = comments <> problem <> clauses
  where
    comments = foldMap bComment (wdimacsComments t)
    problem = bLine [ copyByteString "p wcnf"
                    , integral (wdimacsNumVariables t)
                    , integral (Set.size tClauses)
                    , integral (wdimacsTopWeight t)
                    ]
    clauses = foldMap (uncurry bWClause) tClauses

    tClauses = wdimacsClauses t

bComment :: ByteString -> Builder
bComment bs = bLine [ fromChar 'c', fromByteString bs ]

bClause :: IntSet -> Builder
bClause ls = bLine0 (map integral (IntSet.toList ls))

bWClause :: Int64 -> IntSet -> Builder
bWClause w ls = bLine0 (integral w : map integral (IntSet.toList ls))

bLine0 :: [Builder] -> Builder
bLine0 = bLine . (++ [fromChar '0'])

bLine :: [Builder] -> Builder
bLine bs = mconcat (List.intersperse (fromChar ' ') bs) <> fromChar '\n'

-- | An explicit prenex quantifier
data Quant
  = Exists { getQuant :: {-# UNPACK #-} !Int }
  | Forall { getQuant :: {-# UNPACK #-} !Int }
  deriving Typeable

instance DIMACS SAT where
  dimacsComments _ = []
  dimacsNumVariables s = s^.lastAtom
  dimacsClauses = satClauses

instance QDIMACS QSAT where
  qdimacsComments _ = []
  qdimacsNumVariables q = q^.lastAtom
  qdimacsQuantified q =
    quants [1..q^.lastAtom] (IntSet.toAscList (q^.universals))
    where
      quants :: [Int] -> [Int] -> [Quant]
      quants []     _  = []
      quants (i:is) [] = Exists i : quants is []
      quants (i:is) jjs@(j:js)
        | i == j    = Forall i : quants is js
        | otherwise = Exists i : quants is jjs
  qdimacsClauses = satClauses

satClauses :: HasSAT s => s -> Set IntSet
satClauses s = Set.map clauseSet (formulaSet (s^.formula))
