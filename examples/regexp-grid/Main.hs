module Main where

import Control.Monad
import Data.List (intersperse)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Ersatz

import RegexpGrid.Problem
import RegexpGrid.Types

main :: IO ()
main = do
  (res, msol) <- solveWith cryptominisat problem
  when (res /= Satisfied) (error (show res))
  unless (isJust msol) (error ("Sol: " ++ show msol))
  Just sol <- return msol

  mapM_ putStrLn [ line 6 P00 P06 sol
                 , line 5 P10 P17 sol
                 , line 4 P20 P28 sol
                 , line 3 P30 P39 sol
                 , line 2 P40 P4a sol
                 , line 1 P50 P5b sol
                 , line 0 P60 P6c sol
                 , line 1 P70 P7b sol
                 , line 2 P80 P8a sol
                 , line 3 P90 P99 sol
                 , line 4 Pa0 Pa8 sol
                 , line 5 Pb0 Pb7 sol
                 , line 6 Pc0 Pc6 sol
                 ]

line :: Int -> Pos -> Pos -> Map Pos Char -> String
line spaces start end sol =
  replicate spaces ' ' ++ intersperse ' ' (map (sol Map.!) [start..end])
