import Ersatz
import Ersatz.Bit
import Ersatz.Variable (exists)

import System.Environment (getArgs)
import Control.Monad ( forM_, replicateM )

main :: IO ( )
main = do
  argv <- getArgs
  case argv of
    [ n ] -> mainf ( read n)
    [] -> mainf 50000

mainf n = do
  putStrLn $ unwords [ "n",  show n ]
  (s, mgs) <- solveWith anyminisat $ do
      gs <- replicateM n exists
      forM_ (zip gs $ tail gs) $ \ (x,y) -> assert ( x /== y )
      return (gs :: [Bit])
  case (s, mgs) of
     (Satisfied, Just gs) -> do print $ length $ filter id gs
     _ -> do return ()
