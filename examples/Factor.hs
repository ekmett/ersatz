{-# language LambdaCase #-}

import Ersatz
import Ersatz.Bits
import Control.Monad
import Control.Monad.State

import System.Environment

problem :: (MonadState s m, HasSAT s) => Integer -> m (Bits, Bits, Bits)
problem n = do
  let w = log2 n
  a <- liftM Bits (replicateM w exists)
  b <- liftM Bits (replicateM w exists)
  let c = a * b
  assert (a /== encode   1)
  assert (b /== encode   1)
  assert (c === encode   n)
  return (a,b,c)

log2 n = if n > 1 then succ $ log2 $ div n 2 else 1

main :: IO ()
main = getArgs >>= \ case
  [] -> run 143
  [s] -> run $ read s

run n = do
  putStrLn "Solution:"
  (Satisfied, msol@(Just (a,b,c))) <- solveWith minisat $ problem n
  putStrLn (show a ++ " * " ++ show b ++ " = " ++ show c)
