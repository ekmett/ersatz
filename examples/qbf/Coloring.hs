{-# LANGUAGE CPP #-}
module Main where

import Prelude hiding (not, (&&), and, or)

import Ersatz
import Ersatz.Relation
import Ersatz.Counting

import qualified Data.Array as A
import Data.Array (Array, Ix)

import Control.Monad.State.Lazy (StateT)

import Data.List (tails)

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Functor.Identity
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup(..))
#endif

import Options.Applicative
import Text.Printf (printf)


data Coloring = Coloring Int Int Int

coloring :: Parser Coloring
coloring = Coloring
  <$> argument auto ( metavar "n" <> showDefault <> value 11 <> help "Number of nodes of graph G" )
  <*> argument auto ( metavar "c" <> showDefault <> value 3 <> help "G is not c-colorable" )
  <*> argument auto ( metavar "k" <> showDefault <> value 3 <> help "G has no complete subgraph with k nodes" )

-- | default: Grötzsch graph: not 3-colorable and K3-free (11 nodes)
main :: IO ()
main = execParser options >>= run
  where options = info (coloring <**> helper) fullDesc

run :: Coloring -> IO ()
run (Coloring n c k) = do
  printf "n = %d, c = %d, k = %d\n" n c k
  formulaSize $ problem n c k
  solve $ problem n c k


solve :: StateT QSAT IO (Relation Int Int) -> IO ()
solve p = do
  result <- solveWith depqbf p
  case result of
    (Satisfied, Just r) -> mapM_ putStrLn [table r, show $ edgesA r]
    _                   -> putStrLn "unsat"

-- | @problem n c k@ generates a QBF problem that encodes a graph with @n@ nodes,
-- which is not @c@-colorable and does not contain a complete subgraph with @k@ nodes.
problem :: Monad a => Int -> Int -> Int -> StateT QSAT a (Relation Int Int)
problem n c k = do
  r <- symmetric_relation ((0,0),(n-1,n-1))
  col <- universally_quantified_relation ((0,0),(n-1,c-1))
  assert $ and [
      irreflexive r
    , not $ has_k k r
    , is_coloring col ==> not $ proper col r
    ]
  return r


universally_quantified_relation :: (Ix a, Ix b, MonadQSAT s m)
  => ((a,b),(a,b)) -> m (Relation a b)
universally_quantified_relation bnd = do
  pairs <- sequence $ do
    p <- A.range bnd
    return $ do
      x <- forall_
      return (p,x)
  return $ build bnd pairs

-- | @has_k n r@ encodes the constraint that @r@ has a complete subgraph with @n@ nodes.
has_k :: Ix a => Int -> Relation a a -> Bit
has_k n r = or $ do
  xss <- select n $ universe r
  return $ and $ do
    (x:xs) <- tails xss
    y <- xs
    return $ r!(x,y)

-- | select 2 [1..4] = [[1,2],[1,3],[1,4],[2,3],[2,4],[3,4]]
select :: Int -> [a] -> [[a]]
select 0 _ = [[]]
select _ [] = []
select k (x:xs) = map (x:) (select (k-1) xs) ++ select k xs

-- | Given a relation r with domain a and codomain b, check if r matches every element in a
-- to exactly one element in b.
is_coloring :: Ix a => Relation a a -> Bit
is_coloring c = and $ do
  i <- domain c
  return $ exactly 1 $ do
    j <- codomain c
    return $ c!(i,j)

-- | @proper c r@ encodes the constraint that @c@ is a proper coloring for @r@.
proper :: Ix a => Relation a a -> Relation a a -> Bit
proper col r = and $ do
  (p,q) <- indices r
  j <- codomain col
  return $ r!(p,q) ==> not $ col!(p,j) && col!(q,j)


edgesA :: (Ix a, Ix b) => Array (a,b) Bool -> [(a,b)]
edgesA a = [ i | (i, b) <- A.assocs a, b == True]

formulaSize :: StateT QSAT Identity a -> IO ()
formulaSize p = mapM_ C.putStrLn $ take 2 $ B.split 10 $ qdimacsQSAT p
