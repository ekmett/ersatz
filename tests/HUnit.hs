-- | @HUnit@-based unit tests for @ersatz@.
module Main where

import Prelude hiding ((||), (&&), not)

import Data.Default
import qualified Data.IntMap.Strict as IntMap

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, (@?=), testCase)

import Ersatz
import Ersatz.Internal.Literal

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "unit tests"
    [ testCase "unconstrained literals" case_unconstrained_literals
    ]

-- A regression test for #60 and #76.
case_unconstrained_literals :: Assertion
case_unconstrained_literals =
    decode sol [b1, not b1, b2, not b2, b1 || b2, b1 && b2] @?=
      Just [False, True, False, True, False, False]
      -- There are other valid answers, but in practice, ersatz will choose this
      -- one due to the convention that the Codec Literal instance always
      -- assigns non-negative unconstrained Literals to False and negative
      -- unconstrained Literals to True.
  where
    sol = solutionFrom (IntMap.fromList [(1, True)]) (def :: SAT)
    b1 = Var $ Literal 2
    b2 = Var $ Literal 3
