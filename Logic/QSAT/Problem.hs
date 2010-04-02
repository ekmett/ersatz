{-# LANGUAGE ExistentialQuantification #-}
module Logic.QSAT.Problem
    ( CNF(..)
    , Clause(..)
    , Literal(getLiteral)
    , QDIMACS(..)
    , Not(..)
    , Problem(..)
    , Exists, Forall
    ) where


import Data.Monoid.Reducer
import Control.Monad.Writer.Class
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)

data Exists
data Forall

newtype CNF b = CNF (Seq (Clause b))
    deriving (Monoid)

instance Reducer (Clause b) (CNF b) where
    unit = CNF . singleton
    cons a (CNF s) = CNF (a <| s)
    snoc (CNF s) a = CNF (s |> a)

instance Reducer (Literal b) (CNF b) where
    unit = CNF . singleton . Clause . return 
    cons a (CNF s) = CNF (Clause [a] <| s) 
    snoc (CNF s) a = CNF (s |> Clause [a])
        
newtype Clause b = Clause [Literal]
    deriving (Monoid)

instance Reducer (Literal b) (Clause b) where
    unit = Clause . return
    cons a (Clause as) = Clause (a:as)
    snoc (Clause as) a = Clause (a:as)

newtype Literal b = Literal { getLiteral :: Int } 

instance Show (Literal b) where
    showsPrec i = showsPrec i . getLiteral
    show = show . getLiteral
    showList = showList . map getLiteral

class Not t where
    not :: t -> t

instance Not (Literal b) where
    not = Literal . negate . getLiteral

instance Not Bool where
    not = Prelude.not

data Problem = forall b. Problem {-# UNPACK #-} !Int !(CNF b) !IntSet

class QDIMACS t where
    qdimacs :: t -> String

data Quant = Exists { getQuant :: {-# UNPACK #-} !Int } 
           | Forall { getQuant :: {-# UNPACK #-} !Int } 

instance QDIMACS Problem where
    -- obtain the dimacs format for this problem
    qdimacs :: Problem -> String
    qdimacs (Problem vars (CNF cs) qs) = unlines  $
        unwords ["p","cnf", show vars, show (Seq.length cs) ] : 
        map showGroup quantGroups ++ 
        map qdimacs (toList cs) 

      where

        quantGroups | I.null qs = []
                    | otherwise = groupBy eqQuant quants [1..vars] (I.toAscList qs)

        showGroup xs = unwords $ q (head xs) : map getQuant xs

        eqQuant :: Quant -> Quant -> Bool
        eqQuant Exists{} Exists{} = True
        eqQuant Forall{} Forall{} = True
        eqQuant _ _ = False

        q Exists{} = "e"
        q Forall{} = "a"

        quants [] _ = []
        quants (i:is) []     = Exists i : quants is []
        quants (i:is) jjs@(j:js) 
            | i == j    = Forall i : quants is js
            | otherwise = Exists i : quants is jjs

-- represent a basic SAT problem in CNF
instance QDIMACS (CNF b) where
    qdimacs (CNF s) = unlines $ 
        unwords ["p", "cnf", show (maximum . concatMap getClause $ F.toList s), show (Seq.length s)] :
        map qdimacs (F.toList s)

instance QDIMACS (Clause b) where
    qdimacs (Clause xs) = unwords $ map show xs ++ ["0"]

instance QDIMACS (Literal b) where
    qdimacs (Literal n) = show n
