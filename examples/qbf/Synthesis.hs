-- | An example program that solves a circuit synthesis problem with the Boolean Chain approach,
-- as cited in "Circuit Minimization with QBF-Based Exact Synthesis" by Reichl et al. 2023
-- <https://doi.org/10.1609/aaai.v37i4.25524/>.

module Main where

import Prelude hiding (not, (&&), and, or, product)

import Ersatz
import Ersatz.Relation
import Ersatz.Counting

import qualified Data.Array as A
import Data.Array (Array, Ix)

import Control.Monad.State.Lazy (StateT)
import Control.Monad (guard)

import Text.Printf (printf)
import Data.List (sortOn)

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Functor.Identity


-- | AIG where @1 && 2@ is equivalent to @4@
main :: IO ()
main = do
  let n = 2 -- number of inputs
      l = 2 -- number of steps (gates)
      m = 1 -- number of outputs
      f :: [Bit] -> [Bit]
      f xs = [and xs]
  formulaSize $ problem f n l m
  solve $ problem f n l m

-- | AIG where @atmost 2 [1,2,3,4]@ is equivalent to @10@
aig_atmost :: IO ()
aig_atmost = do
  let n = 4 -- number of inputs
      l = 6 -- number of steps (gates)
      m = 1 -- number of outputs
      f :: [Bit] -> [Bit]
      f xs = [atmost 2 xs]
  formulaSize $ problem f n l m
  solve $ problem f n l m

-- | AIG where @exactly 2 [1,2,3]@ is equivalent to @8@
aig_exactly :: IO ()
aig_exactly = do
  let n = 3 -- number of inputs
      l = 5 -- number of steps (gates)
      m = 1 -- number of outputs
      f :: [Bit] -> [Bit]
      f xs = [exactly 2 xs]
  formulaSize $ problem f n l m
  solve $ problem f n l m


solve :: StateT QSAT IO (Relation Int Int, Relation Int Int) -> IO ()
solve p = do
  result <- solveWith (depqbfPathArgs "depqbf" ["--qdo", "--no-dynamic-nenofex"]) $ p
  case result of
    (Satisfied, Just (s,o)) -> printf "output: %s\nAIG:\n%s%s\n" (show $ map fst $ sortOn snd $ edgesA o) (table s) (show $ edgesA s)
    _                       -> putStrLn "unsat"

-- | @problem f n l m@ generates a QBF problem that encodes an AIG graph with @n@ inputs,
-- @l@ gates and @m@ outputs which is equivalent to the boolean function @f@.
problem :: Monad a => ([Bit] -> [Bit]) -> Int -> Int -> Int -> StateT QSAT a (Relation Int Int, Relation Int Int)
problem f n l m = do
  s <- relation ((n+1,1),(n+l,n+l)) -- selection variables: (x,y) is in s iff gate x takes gate/input y as an input
  assert $ and $ do -- ensure that s is acyclic (DAG)
    (i,j) <- indices s
    guard $ (i <= j)
    return $ s!(i,j) === false
  o <- relation ((1,1),(n+l,m)) -- output variables: (x,y) is in o iff output y is gate/input x
  assert $ regular_in_degree 1 o
  v <- universally_quantified_relation ((1,1),(1,n)) -- input variables: (1,y) is in v iff input y is true
  g <- relation ((1,1),(1,n+l)) -- gate value variables: (1,y) is in g iff gate/input y is true
  assert $ and $ do
    i <- [1..n]
    return $ g!(1,i) === v!(1,i)
  assert $ and $ do -- g!(1,y) = nand xs where xs is the list of inputs of y
    i <- [n+1..n+l]
    let val = not $ and $ do
          p <- [1..i-1]
          return $ s!(i,p) ==> g!(1,p)
    return $ g!(1,i) === val
  assert $ f (elems v) === elems (product g o)
  return (s,o)


universally_quantified_relation :: (Ix a, Ix b, MonadQSAT s m)
  => ((a,b),(a,b)) -> m (Relation a b)
universally_quantified_relation bnd = do
  pairs <- sequence $ do
    p <- A.range bnd
    return $ do
      x <- forall_
      return (p,x)
  return $ build bnd pairs


edgesA :: (Ix a, Ix b) => Array (a,b) Bool -> [(a,b)]
edgesA a = [ i | (i, b) <- A.assocs a, b == True]

formulaSize :: StateT QSAT Identity a -> IO ()
formulaSize p = mapM_ C.putStrLn $ take 2 $ B.split 10 $ qdimacsQSAT $ p
