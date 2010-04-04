{-# LANGUAGE Rank2Types, GeneralizedNewtypeDeriving, TypeOperators, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, PatternGuards #-}
module Data.Logic.Ersatz.Internal.Problem
    ( QBF(qbfLastAtom, qbfClauses, qbfUniversals), emptyQBF
    , Clause(..), clauseLiterals
    , Clauses
    , Literal(literalId), negateLiteral 
    , Lit(..), lit, negateLit, litExists, litForall
    , QDIMACS(..)
    , SAT(..)
    , MonadSAT(..)
    , Variable(..)
    , (:*:)(..)
    , assertLits, assertNamedLits
    -- , assume
    -- , reifyLit
    ) where

import Control.Applicative
-- import Control.Monad (ap)
import Control.Monad.ST
import Control.Monad.State

-- import qualified Data.Sequence as Seq
-- import Data.Sequence (Seq)

import Data.Foldable (Foldable, foldMap)
import Data.Traversable (Traversable, traverse)
import qualified Data.Traversable as Traversable

-- import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import Data.Map (Map)
import qualified Data.Map as Map


-- import Data.Set (Set)
-- import qualified Data.Set as Set

-- import Data.Maybe (maybeToList)

import qualified Data.List as List (groupBy, partition)

import Data.Logic.Ersatz.Internal.Reify

import Data.Monoid

-- | (Q)QDIMACS file format pretty printer
class QDIMACS t where
    qdimacs :: t s -> String

instance QDIMACS Literal where
    qdimacs (Literal n) = show n

-- | A naked possibly-negated Atom, present in the target solver.
newtype Literal s = Literal { literalId :: Int } deriving (Eq,Ord)

instance Show (Literal b) where
    showsPrec i = showsPrec i . literalId
    show = show . literalId
    showList = showList . map literalId

negateLiteral :: Literal b -> Literal b
negateLiteral = Literal . negate . literalId

-- | Literals with partial evaluation
data Lit s
    = Lit  { getLiteral  :: {-# UNPACK #-} !(Literal s) }
    | Bool { getValue :: !Bool }

instance Variable Lit where
    known Bool{} = True
    known _      = False
    exists = litExists
    forall = litForall

litExists :: MonadSAT s m => m (Lit s)
litExists = Lit <$> exists

litForall  :: MonadSAT s m => m (Lit s)
litForall = Lit <$> forall

lit :: Bool -> Lit b
lit = Bool

negateLit :: Lit s -> Lit s
negateLit (Bool b) = Bool (not b)
negateLit (Lit l) = Lit (negateLiteral l)

-- type Solver t m = forall b. t b -> m (Solution b)
-- newtype Solution b = Solution { solutionMap :: IntMap Bool } 

newtype Clause s = Clause { clauseSet :: IntSet } deriving (Eq, Ord, Monoid)

instance QDIMACS Clause where
    qdimacs (Clause xs) = unwords $ map show (IntSet.toList xs) ++ ["0"]

clauseLiterals :: Clause s -> [Literal s]
clauseLiterals (Clause is) = Literal <$> IntSet.toList is

type Clauses s = Map (Clause s) (Maybe String)

data QBF s = QBF 
    { qbfLastAtom   :: {-# UNPACK #-} !Int      -- ^ The id of the last atom allocated
    , qbfClauses    :: !(Clauses s)             -- ^ a map of clauses to assert to names
    , qbfUniversals :: !IntSet                  -- ^ a set indicating which literals are universally quantified
    , qbfLitMap     :: !(DynStableMap (Lit s))  -- ^ a mapping used during 'Bit' expansion
--  , qbfNameMap    :: !(IntMap String)         -- ^ a map of literals to given names
    } 

-- provided for convenience
instance Show (QBF s) where
    show = qdimacs

emptyQBF :: QBF s
emptyQBF = QBF 0 Map.empty IntSet.empty IntMap.empty

{-
class Annotated t where
    (<?>) :: t -> String -> t 

instance Annotated (SAT s (Literal s)) where 
    m <?> name = SAT $ do
        modify $ \qbf { qbfNameMap = IntMap.Insert (literalId m) name (qbfNameMap qbf) }
-}

newtype SAT s a = SAT { runSAT :: StateT (QBF s) (ST s) a } 
    deriving (Functor,Monad)

-- We can't rely on having an Applicative instance for StateT st (ST s)
instance Applicative (SAT s) where
    pure = return
    (<*>) = ap

class (Applicative m, Monad m) => MonadSAT s m | m -> s where
    literalExists :: m (Literal s)
    literalForall :: m (Literal s)
    assertClause  :: Clause s -> Maybe String -> m ()
    liftST        :: ST s a -> m a
    insertDyn     :: DynStableName -> Lit s -> m ()
    lookupDyn     :: DynStableName -> m (Maybe (Lit s))

instance MonadSAT s (SAT s) where
    literalExists = SAT $ do
        qbf <- get 
        let qbfLastAtom' = qbfLastAtom qbf + 1
            qbf' = qbf { qbfLastAtom = qbfLastAtom' }
        put qbf'
        return (Literal qbfLastAtom')

    assertClause clause name = SAT $ do
        modify $ \ qbf -> qbf { qbfClauses = Map.insertWith mplus clause name (qbfClauses qbf) } 

    liftST = SAT . lift

    insertDyn k v = SAT $ modify $
        \qbf -> qbf { qbfLitMap = insertDynStableMap k v (qbfLitMap qbf) }

    lookupDyn k = SAT $ lookupDynStableMap k <$> gets qbfLitMap 

    literalForall = SAT $ do
        qbf <- get 
        let qbfLastAtom' = qbfLastAtom qbf + 1
            qbf' = qbf { qbfLastAtom = qbfLastAtom'
                       , qbfUniversals = IntSet.insert qbfLastAtom' (qbfUniversals qbf) 
                       }
        put qbf'
        return (Literal qbfLastAtom')
    
class Variable t where
    known  :: t s -> Bool
    exists :: MonadSAT s m => m (t s)
    forall :: MonadSAT s m => m (t s)

instance Variable Literal where
    known _ = False
    exists = literalExists
    forall = literalForall

-- functor product
data (f :*: g) a = f a :*: g a

instance (Functor f, Functor g) => Functor (f :*: g) where
    fmap f (a :*: b) = fmap f a :*: fmap f b

instance (Foldable f, Foldable g) => Foldable (f :*: g) where
    foldMap f (a :*: b) = foldMap f a `mappend` foldMap f b

instance (Traversable f, Traversable g) => Traversable (f :*: g) where
    traverse f (a :*: b) = (:*:) <$> traverse f a <*> traverse f b

instance (Variable f, Variable g) => Variable (f :*: g) where
    known (f :*: g) = known f && known g
    exists = (:*:) <$> exists <*> exists
    forall = (:*:) <$> forall <*> forall

assertLits :: MonadSAT s m => [Lit s] -> m ()
assertLits lits 
    | any getValue knowns = return ()
    | otherwise = assertClause (Clause literalSet) Nothing
    where (knowns, unknowns) = List.partition known lits
          literalSet = IntSet.fromList $ map (literalId . getLiteral) unknowns

assertNamedLits :: MonadSAT s m => [Lit s] -> String -> m ()
assertNamedLits lits name
    | any getValue knowns = return ()
    | otherwise = assertClause (Clause literalSet) (Just name)
    where (knowns, unknowns) = List.partition known lits
          literalSet = IntSet.fromList $ map (literalId . getLiteral) unknowns


{-
assume :: IntMap Bool -> Clauses b -> Clauses b
assume knowns map = IntMap.fromListWith mplus $ do 
        (k,v) <- assocs map
        let k' = IntSet.fromAscList $ go knowns [] $ IntSet.toDescList k
        return (k',v)
    where 
        -- reverse the list into an accumulating parameter. 
        -- and filter elements that are known and for which the literal evaluates
        -- to to true. if anyfalsehoods are found, the whole conjunct fails.
        -- so we map it to the empty conjunct
        go :: IntMap Bool -> [Int] -> [Int] -> [Int]
        go _ acc [] = acc
        go ks acc (n:xs) 
            | n < 0 = case IntMap.lookup (negate n) ks of
                Nothing -> go ks (n:acc) xs -- keep it
                Just True -> []             -- destroy the earth
                Just False -> go ks acc xs  -- filter it
            | otherwise = case IntMap.lookup n ks of
                Nothing -> go ks (n:acc) xs -- keep it
                Just True -> go ks acc xs   -- filter it
                Just False -> []            -- destroy the earth
-}

-- this allocates too many literals
{-
reifyLit :: (MonadSAT s m, MuRef f) => f s -> m (Lit s)
reifyLit a = a `seq` do
        k <- liftST $ makeDynStableName root
        l <- lookupDyn k 
        case l of
            Nothing -> do
                v <- exists -- Lit
                insertDyn k v
                mapDeRef reifyLit a
                return v
            Just v -> return v
-}


data Quant = Exists { getQuant :: {-# UNPACK #-} !Int }
           | Forall { getQuant :: {-# UNPACK #-} !Int }

instance QDIMACS QBF where
    qdimacs (QBF vars cs qs _) = unlines  $
        unwords ["p","cnf", show (vars + padding), show (Map.size cs) ] :
        map showGroup quantGroups ++
        map qdimacs (Map.keys cs)
      where
        -- "The innermost quantified set is always of type 'e'" per QDIMACS standard
        padding | Just (n, _) <- IntSet.maxView qs, n == vars = 1
                | otherwise                                   = 0 
                      -- no universals means we are a plan DIMACS file
        quantGroups | IntSet.null qs = []
                      -- otherwise, skip to the first universal and show runs
                    | otherwise = List.groupBy eqQuant $ quants [head qlist..vars] qlist
            where qlist = IntSet.toAscList qs

        showGroup :: [Quant] -> String
        showGroup xs = unwords $ q (head xs) : map (show . getQuant) xs

        eqQuant :: Quant -> Quant -> Bool
        eqQuant Exists{} Exists{} = True
        eqQuant Forall{} Forall{} = True
        eqQuant _ _ = False

        q :: Quant -> String
        q Exists{} = "e"
        q Forall{} = "a"

        quants :: [Int] -> [Int] -> [Quant]
        quants [] _ = []
        quants (i:is) []     = Exists i : quants is []
        quants (i:is) jjs@(j:js)
            | i == j    = Forall i : quants is js
            | otherwise = Exists i : quants is jjs

