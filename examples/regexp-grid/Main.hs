module Main (main) where

import Control.Monad
import Data.List (intersperse)
import Data.Map (Map)
import qualified Data.Map as Map
import Ersatz

import RegexpGrid.Problem
import RegexpGrid.Types

main :: IO ()
main = do
  (res, msol) <- solveWith cryptominisat problem
  when (res /= Satisfied) (fail (show res))
  case msol of
    Nothing  -> fail "Sol was Nothing"
    Just sol ->
      mapM_ (putStrLn . ($ sol))
        [ line 6 P00 P06
        , line 5 P10 P17
        , line 4 P20 P28
        , line 3 P30 P39
        , line 2 P40 P4a
        , line 1 P50 P5b
        , line 0 P60 P6c
        , line 1 P70 P7b
        , line 2 P80 P8a
        , line 3 P90 P99
        , line 4 Pa0 Pa8
        , line 5 Pb0 Pb7
        , line 6 Pc0 Pc6
        ]

line :: Int -> Pos -> Pos -> Map Pos Char -> String
line spaces start end sol =
  replicate spaces ' ' ++ intersperse ' ' (map (sol Map.!) [start..end])
