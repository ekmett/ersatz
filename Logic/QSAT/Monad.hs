module Logic.QSAT.Monad
    ( QSAT(..)
    , problem
    , runQSAT
    , fresh
    , all 
    , tellClause
    ) where

import Data.Monoid.Reducer
import Control.Monad.Writer.Class
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Logic.QSAT.Problem

-- the IntSet contains all of the universally quantified variables
newtype QSAT b a = QSAT { runQSAT :: forall r. (a -> Int -> CNF b -> IntSet -> r) -> Int -> CNF b -> IntSet -> r }

-- give a copy of the problem so far
problem :: QSAT b Problem
problem = QSAT (\k f qs cs -> k (Problem f qs cs) f qs cs)

runQSAT :: (forall b. QSAT b a) -> (a, Problem)
runQSAT (QSAT k) = k (\a f cs qs -> (a, Problem f cs qs)) 0 mempty memtpy

fresh :: QSAT b (Literal b)
fresh = QSAT (\k f -> let f' = f + 1 in f' `seq` k (Literal f') f')

all :: QSAT b (Literal b)
all n = QSAT (\k f qs -> let f' = f + 1
                             qs' = insert (abs (getLiteral n)) qs 
                         in f' `seq` qs' `seq` k n f' qs')

tellClause :: Clause b -> QSAT b ()
tellClause c = QSAT (\k f qs (CNF cs) -> let cs' = CNF (cs |> c) in cs' `seq` k () f qs cs')

instance MonadWriter (CNF b) (QSAT b) where
    tell cs' = QSAT (\k f qs (CNF cs) -> let cs'' = CNF (cs >< cs') in cs'' `seq` 
                                        k () f qs cs'')
    listen (QSAT m) = QSAT (\k -> m (\a f qs cs -> k (a,cs) f qs cs))
    pass (QSAT m) = QSAT (\k -> m (\(a, p) f qs w -> let w' = p w in w' `seq` k a f qs w')))
    
instance Functor (QSAT b) where
    fmap f (QSAT m) = QSAT (\k -> m (\x -> k (f x)))

instance Applicative (QSAT b) where
    pure a = QSAT (\k -> k a)
    QSAT f <*> QSAT x = QSAT (\k -> f (\f' -> x (\x' -> k (f' x'))))

instance Monad (QSAT b) where
    return a = QSAT (\k -> k a)
    QSAT c >>= f = QSAT (\k -> c (\a -> runQSAT (f a) k))

-- TODO: treak the argument b like a monadic region parameter, so we can safely access parent problem variables
-- data Fork child parent
-- promote :: Promotable f => f b -> f (Fork b' b)
-- forkQSAT :: (forall b'. QSAT (Fork b' b) a) -> QSAT b (a, Problem)
-- forkQSAT (QSAT m) -> QSAT (\k f qs cs -> m (\k' f' qs' cs' -> 
