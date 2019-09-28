module Main where

import Ersatz
import Control.Monad

problem :: MonadSAT s m => m (Bits, Bits, Bits)
problem = do
  a <- liftM Bits (replicateM 5 exists)
  b <- liftM Bits (replicateM 5 exists)
  let c = a * b
  assert (a /== encode   1)
  assert (b /== encode   1)
  assert (c === encode 143)
  return (a,b,c)

main :: IO ()
main = do
  putStrLn "Solution:"
  (Satisfied, Just (a,b,c)) <- solveWith cryptominisat5 problem
  putStrLn (show a ++ " * " ++ show b ++ " = " ++ show c)
