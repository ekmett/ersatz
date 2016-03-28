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
    [ d, n ] -> mainf (read d)( read n)
    [] -> mainf 3 150

mainf d n = do
  putStrLn $ unwords [ "degree ==", show d, "nodes ==", show n ]
  (s, mg) <- solveWith minisat $ check d n
  case (s, mg) of
     (Satisfied, Just g) -> do printA g 
     _ -> do return ()

check :: (MonadState s m, HasSAT s )
      => Int -> Int 
      -> m (R.Relation Int Int)
check d n = do
  g <- R.relation ((0,0),(n-1,n-1))
  assert $ R.regular_out_degree (d+1) g 
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

