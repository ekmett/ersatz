import Ersatz
import Ersatz.BitN
import Control.Monad
import Control.Monad.State

problem :: (MonadState s m, HasSAT s) => m (BitN, BitN, BitN)
problem = do
  a <- liftM BitN (replicateM 5 exists)
  b <- liftM BitN (replicateM 5 exists)
  let c = a * b
  assert (a /== encode   1)
  assert (b /== encode   1)
  assert (c === encode 143)
  return (a,b,c)

main :: IO ()
main = do
  putStrLn "Solution:"
  (Satisfied, msol@(Just (a,b,c))) <- solveWith minisat problem
  putStrLn (show a ++ " * " ++ show b ++ " = " ++ show c)
