{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ConstraintKinds #-}

{-# OPTIONS_HADDOCK not-home #-}
--------------------------------------------------------------------
-- |
-- Copyright :  © Edward Kmett 2010-2014, Johan Kiviniemi 2013
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
  , MonadSAT
  , runSAT, runSAT', dimacsSAT
  , literalExists
  , assertFormula
  , generateLiteral
  -- * QSAT
  , QSAT(QSAT)
  , HasQSAT(..)
  , MonadQSAT
  , runQSAT, runQSAT', qdimacsQSAT
  , literalForall
  -- * DIMACS pretty printing
  , DIMACS(..)
  , QDIMACS(..)
  , WDIMACS(..)
  , dimacs, qdimacs, wdimacs
  , writeDimacs, writeQdimacs, writeWdimacs
  , writeDimacs', writeQdimacs', writeWdimacs'
  ) where

import Data.ByteString.Builder
import Control.Lens
import Control.Monad.State
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Lazy
import Data.Default
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.Int
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import Ersatz.Internal.Formula
import Ersatz.Internal.Literal
import Ersatz.Internal.StableName
import System.IO ( withFile, IOMode(WriteMode) )
import System.IO.Unsafe
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup(..))
#endif

-- | Constraint synonym for types that carry a SAT state.
type MonadSAT  s m = (HasSAT  s, MonadState s m)

-- | Constraint synonym for types that carry a QSAT state.
type MonadQSAT s m = (HasQSAT s, MonadState s m)

------------------------------------------------------------------------------
-- SAT Problems
------------------------------------------------------------------------------

data SAT = SAT
  { _lastAtom  :: {-# UNPACK #-} !Int      -- ^ The id of the last atom allocated
  , _formula   :: !Formula                 -- ^ a set of clauses to assert
  , _stableMap :: !(HashMap (StableName ()) Literal)  -- ^ a mapping used during 'Bit' expansion
  }

class HasSAT s where
  sat :: Lens' s SAT

  lastAtom :: Lens' s Int
  lastAtom f = sat $ \ (SAT a b c) -> fmap (\a' -> SAT a' b c) (f a)

  formula :: Lens' s Formula
  formula f = sat $ \ (SAT a b c) -> fmap (\b' -> SAT a b' c) (f b)

  stableMap :: Lens' s (HashMap (StableName ()) Literal)
  stableMap f = sat $ \ (SAT a b c) -> SAT a b <$> f c

instance HasSAT SAT where
  sat = id

instance Show SAT where
  showsPrec p bf = showParen (p > 10)
                 $ showString "SAT " . showsPrec 11 (bf^.lastAtom)
                 . showChar ' ' . showsPrec 11 (bf^.formula)
                 . showString " mempty"


instance Default SAT where
  -- The literal 1 is dedicated for the True constant.
  def = SAT 1 (formulaLiteral literalTrue) HashMap.empty

-- | Run a 'SAT'-generating state computation. Useful e.g. in ghci for
-- disambiguating the type of a 'MonadSAT' value.
runSAT :: StateT SAT m a -> m (a, SAT)
runSAT s = runStateT s def

-- | Run a 'SAT'-generating state computation in the 'Identity' monad. Useful
-- e.g. in ghci for disambiguating the type of a 'MonadSAT' value.
runSAT' :: StateT SAT Identity a -> (a, SAT)
runSAT' = runIdentity . runSAT

-- | Run a 'SAT'-generating state computation and return the respective
-- 'DIMACS' output. Useful for testing and debugging.
dimacsSAT :: StateT SAT Identity a -> Lazy.ByteString
dimacsSAT = toLazyByteString . dimacs . snd . runSAT'

literalExists :: MonadSAT s m => m Literal
literalExists = fmap Literal $ lastAtom <+= 1
{-# INLINE literalExists #-}

assertFormula :: MonadSAT s m => Formula -> m ()
assertFormula xs = formula <>= xs
{-# INLINE assertFormula #-}

generateLiteral :: MonadSAT s m => a -> (Literal -> m ()) -> m Literal
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
  } deriving Show

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

literalForall :: MonadQSAT s m => m Literal
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
  dimacsClauses :: t -> Seq IntSet

-- | QDIMACS file format pretty printer
--
-- This is used to generate the problem statement for a given 'QSAT' 'Ersatz.Solver.Solver'.

-- http://www.qbflib.org/qdimacs.html
class QDIMACS t where
  qdimacsComments :: t -> [ByteString]
  qdimacsNumVariables :: t -> Int
  qdimacsQuantified :: t -> [Quant]
  qdimacsClauses :: t -> Seq IntSet

-- | WDIMACS file format pretty printer
--
-- This is used to generate the problem statement for a given 'MaxSAT' 'Ersatz.Solver.Solver' (TODO).

-- http://maxsat.ia.udl.cat/requirements/
class WDIMACS t where
  wdimacsComments :: t -> [ByteString]
  wdimacsNumVariables :: t -> Int
  wdimacsTopWeight :: t -> Int64  -- ^ Specified to be 1 ≤ n < 2^63
  wdimacsClauses :: t -> Seq (Int64, IntSet)

-- | Generate a 'Builder' out of a 'DIMACS' problem.
dimacs :: DIMACS t => t -> Builder
dimacs t = comments <> problem <> clauses
  where
    comments = foldMap bComment (dimacsComments t)
    problem = bLine [ string7 "p cnf"
                    , intDec (dimacsNumVariables t)
                    , intDec (Seq.length tClauses)
                    ]
    clauses = foldMap bClause tClauses

    tClauses = dimacsClauses t

-- | Generate a 'Builder' out of a 'QDIMACS' problem.
qdimacs :: QDIMACS t => t -> Builder
qdimacs t = comments <> problem <> quantified <> clauses
  where
    comments = foldMap bComment (qdimacsComments t)
    problem = bLine [ string7 "p cnf"
                    , intDec (qdimacsNumVariables t)
                    , intDec (Seq.length tClauses)
                    ]
    quantified = foldMap go tQuantGroups
      where go ls = bLine0 (q (NE.head ls) : map (intDec . getQuant) (NE.toList ls))
            q Exists{} = char7 'e'
            q Forall{} = char7 'a'
    clauses = foldMap bClause tClauses

    tQuantGroups = NE.groupBy eqQuant (qdimacsQuantified t)
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
    problem = bLine [ string7 "p wcnf"
                    , intDec (wdimacsNumVariables t)
                    , intDec (Seq.length tClauses)
                    , int64Dec (wdimacsTopWeight t)
                    ]
    clauses = foldMap (uncurry bWClause) tClauses

    tClauses = wdimacsClauses t

bComment :: ByteString -> Builder
bComment bs = bLine [ char7 'c', byteString bs ]

bClause :: IntSet -> Builder
bClause = IntSet.foldl' ( \ e i -> intDec i <> char7 ' ' <> e ) ( char7 '0' <> char7 '\n' )

bWClause :: Int64 -> IntSet -> Builder
bWClause w ls = bLine0 (int64Dec w : map intDec (IntSet.toList ls))

bLine0 :: [Builder] -> Builder
bLine0 = bLine . (++ [char7 '0'])

bLine :: [Builder] -> Builder
bLine bs = mconcat (List.intersperse (char7 ' ') bs) <> char7 '\n'

-- | An explicit prenex quantifier
data Quant
  = Exists { getQuant :: {-# UNPACK #-} !Int }
  | Forall { getQuant :: {-# UNPACK #-} !Int }

instance DIMACS SAT where
  dimacsComments _ = []
  dimacsNumVariables s = s^.lastAtom
  dimacsClauses = satClauses

instance QDIMACS QSAT where
  qdimacsComments _ = []
  qdimacsNumVariables q = q^.lastAtom + padding
    where
      -- "The innermost quantified set is always of type 'e'" per QDIMACS
      -- standard. Add an existential atom if the last one is universal.
      padding
        | Just (i, _) <- IntSet.maxView (q^.universals), i == q^.lastAtom = 1
        | otherwise = 0
  -- "Unbound atoms are to be considered as being existentially quantified in
  -- the first (i.e., the outermost) quantified set." per QDIMACS standard.
  -- Skip the implicit first existentials.
  qdimacsQuantified q =
    case qUniversals of
      []    -> []
      (i:_) -> quants [i..lastAtom'] qUniversals
    where
      lastAtom'   = qdimacsNumVariables q
      qUniversals = IntSet.toAscList (q^.universals)

      quants :: [Int] -> [Int] -> [Quant]
      quants []     _  = []
      quants (i:is) [] = Exists i : quants is []
      quants (i:is) jjs@(j:js)
        | i == j    = Forall i : quants is js
        | otherwise = Exists i : quants is jjs
  qdimacsClauses = satClauses

-- | the name is wrong (Does it return Clauses? No - it returns IntSets.)
-- and it means extra work (traversing, and re-building, the full collection).
-- Or is this fused away (because of Coercible)?
satClauses :: HasSAT s => s -> Seq IntSet
satClauses s = fmap clauseSet (formulaSet (s^.formula))


------------------------------------------------------------------------------
-- Writing SATs
------------------------------------------------------------------------------

writeBuilder :: MonadIO m => FilePath -> Builder -> m ()
writeBuilder path builder = liftIO $ do
  withFile path WriteMode $ \fh ->
    hPutBuilder fh builder

-- | Write a 'DIMACS' problem to a file at a particular path. Useful if you want
-- to experiment with solvers, solver options, or measure effects of different encodings.
--
-- @writeDimacs path m@ can be used in the same contexts you would call
-- @solveWith solverName m@.
writeDimacs :: (MonadIO m, DIMACS s, Default s) => FilePath -> StateT s m a -> m ()
writeDimacs path m = writeDimacs' path . snd =<< runStateT m def

-- | Write a 'QDIMACS' problem to a file at a particular path. Useful if you want
-- to experiment with solvers, solver options, or measure effects of different encodings.
--
-- @writeQDimacs path m@ can be used in the same contexts you would call
-- @solveWith solverName m@.
writeQdimacs :: (MonadIO m, QDIMACS s, Default s) => FilePath -> StateT s m a -> m ()
writeQdimacs path m = writeQdimacs' path . snd =<< runStateT m def

-- | Write a 'WDIMACS' problem to a file at a particular path. Useful if you want
-- to experiment with solvers, solver options, or measure effects of different encodings.
--
-- | @writeWdimacs path m@ can be used in the same contexts you would call
-- @solveWith solverName m@
writeWdimacs :: (MonadIO m, WDIMACS s, Default s) => FilePath -> StateT s m a -> m ()
writeWdimacs path m = writeWdimacs' path . snd =<< runStateT m def

-- | Write a 'DIMACS' problem to a file at a particular path. Useful if you want
-- to experiment with solvers, solver options, or measure effects of different encodings.
writeDimacs' :: (MonadIO m, DIMACS t) => FilePath -> t -> m ()
writeDimacs' path = writeBuilder path . dimacs

-- | Write a 'QDIMACS' problem to a file at a particular path. Useful if you want
-- to experiment with solvers, solver options, or measure effects of different encodings.
writeQdimacs' :: (MonadIO m, QDIMACS t) => FilePath -> t -> m ()
writeQdimacs' path = writeBuilder path . qdimacs

-- | Write a 'WDIMACS' problem to a file at a particular path. Useful if you want
-- to experiment with solvers, solver options, or measure effects of different encodings.
writeWdimacs' :: (MonadIO m, WDIMACS t) => FilePath -> t -> m ()
writeWdimacs' path = writeBuilder path . wdimacs
