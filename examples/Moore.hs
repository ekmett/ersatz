-- | graphs n nodes of degree <= d and diameter <= k 
-- see http://combinatoricswiki.org/wiki/The_Degree_Diameter_Problem_for_General_Graphs

-- usage: ./Moore d k n

{-# language FlexibleContexts #-}

import Prelude hiding ( not, or, and )
import qualified Prelude

import Ersatz
import Ersatz.Bit
import qualified Ersatz.Relation as R

import qualified Data.Array as A
import System.Environment (getArgs)
import Control.Monad ( void, when, forM )
import Control.Monad.State

main :: IO ( )
main = do
  argv <- getArgs
  case argv of
    [ d, k, n ] -> void $ mainf ( read d ) (read k) (read n) 
    [ d, k ] -> do
      let go d k n = do
            ok <- mainf d k n 
            when ok $ go d k (n+1) 
      go (read d) (read k) 1 
    [] -> void $ mainf 3 2 10 -- petersen

mainf d k n = do
  putStrLn $ unwords [ "degree <=", show d, "diameter <=", show k, "nodes ==", show n ]
  (s, mg) <- solveWith minisat $ moore d k n 
  case (s, mg) of
     (Satisfied, Just g) -> do printA g ; return True
     _ -> do return False

moore :: (MonadState s m, HasSAT s )
      => Int -> Int -> Int 
      -> m (R.Relation Int Int)
moore d k n = do
  g <- R.symmetric_relation ((0,0),(n-1,n-1))
  assert $ R.reflexive g 
  assert $ R.max_in_degree (d+1) g 
  assert $ R.max_out_degree (d+1) g 
  let p = R.power k g
  assert $ R.complete p 
  return g

-- | FIXME: this needs to go into a library
printA :: A.Array (Int,Int) Bool -> IO ()
printA a = putStrLn $ unlines $ do
         let ((u,l),(o,r)) = A.bounds a
         x <- [u .. o]
         return $ unwords $ do 
             y <- [ l ..r ]
             return $ case a A.! (x,y) of
                  True -> "* " ; False -> ". "

