-- | graphs n nodes of degree <= d and diameter <= k 
-- see http://combinatoricswiki.org/wiki/The_Degree_Diameter_Problem_for_General_Graphs

-- usage: ./Moore d k n [s]
-- d : degree
-- k : diameter
-- n : nodes,
-- s : modulus for symmetry (see periodic_relation below)
-- s smaller => faster (more symmetries) but may lose solutions

-- test cases:  3 2 10 2  -- petersen graph
--              5 2 24    


{-# language FlexibleContexts #-}

import Prelude hiding ( not, or, and )
import qualified Prelude

import Ersatz
import Ersatz.Bit
import qualified Ersatz.Relation as R

import qualified Data.Array as A
import System.Environment (getArgs)
import Control.Monad ( void, when, forM )

main :: IO ( )
main = do
  argv <- getArgs
  case argv of
    [ d, k, n, s ] ->
      void $ mainf ( read d ) (read k) (read n) (read s)
    [ d, k, n    ] ->
      void $ mainf ( read d ) (read k) (read n) (read n)
    [] -> void $ mainf 3 2 10 5 -- petersen

mainf d k n s = do
  putStrLn $ unwords [ "degree <=", show d, "diameter <=", show k, "nodes ==", show n, "symmetry ==", show s ]
  (s, mg) <- solveWith anyminisat $ moore d k n s
  case (s, mg) of
     (Satisfied, Just g) -> do putStrLn $ R.table g ; return True
     _ -> do return False

moore ::
  MonadSAT s m =>
  Int -> Int -> Int -> Int ->
  m (R.Relation Int Int)
moore d k n s = do
  -- g <- R.symmetric_relation ((0,0),(n-1,n-1))
  g <- periodic_relation s ((0,0),(n-1,n-1))
  assert $ R.symmetric g
  assert $ R.reflexive g 
  assert $ R.max_in_degree (d+1) g 
  assert $ R.max_out_degree (d+1) g 
  let p = R.power k g
  assert $ R.complete p 
  return g

periodic_relation s bnd = do
  r <- R.relation bnd
  let normal (x,y) =
        if (x >= s Prelude.&& y >= s)
        then normal (x-s,y-s)  else (x,y)
  return $ R.build bnd $ do
    i <- A.range bnd
    return (i, r R.! normal i)

