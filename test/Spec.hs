-- SPDX-License-Identifier: MIT
-- Copyright (C) 2021 Roland Csaszar
--
-- Project:  TestHaskell
-- File:     Spec.hs
-- Date:     14.May.2021
--
--------------------------------------------------------------------------------

import Lib (fibBetter, fibN)
import System.IO.Unsafe (unsafePerformIO)
import qualified Test.Hspec.Expectations.Pretty as P
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.ExpectedFailure (expectFailBecause)
import qualified Test.Tasty.HUnit as HU (Assertion, assertFailure, testCase)
import qualified Test.Tasty.Hspec as HS
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.SmallCheck as SC
import Text.Printf (printf)

-- | The test program's main entry point, `main`.
main :: IO ()
main = defaultMain tests

-- | The tests.
tests :: TestTree
tests = testGroup "TestHaskell" [fibonacci, goldenRatio]

-- | All tests to check the list of fibonacci numbers.
fibonacci :: TestTree
fibonacci =
  testGroup
    "Test Fibonacci"
    [ -- [ expectFailBecause
      --     "Not Fibonacci!"
      --     (HU.testCase "Must fail: not a Fibonacci series!" $ checkFibonacci [1 .. 100]),
      unsafePerformIO
        ( HS.testSpec "Fibonacci" $
            do
              spec_fibonacci
        )
    ]

spec_fibonacci :: HS.Spec
spec_fibonacci = do
  HS.describe "Must fail: not a Fibonacci series!" $
    HS.it "Second Text?" $ checkFibonacci [1 .. 100]

  HS.describe "Naive version: Check if an+1 = an + an-1, n < 37" $
    HS.it "Second Text?" $ checkFibonacci $ fibN 38

  HS.describe "Better version: Check if an+1 = an + an-1, n < 1000 " $
    HS.it "Second Text?" $ checkFibonacci $ fibBetter 1000

-- | Test the iterative calculation of the golden ration using Fibonacci numbers
--  actually works ;).
goldenRatio :: TestTree
goldenRatio = testGroup "Golden Ratio" []

-- | Test, if for the given list `an+1 = an + an-1` holds for all elements.
checkFibonacci :: [Integer] -> P.Expectation
checkFibonacci list =
  case list of
    [] -> list `P.shouldBe` [1 :: Integer]
    [_] -> list `P.shouldBe` [1 :: Integer, 1 :: Integer]
    [_, _] -> True `P.shouldBe` True
    x : y : z : xs
      | z == x + y -> checkFibonacci (y : z : xs)
      | otherwise -> x + y `P.shouldBe` z -- HU.assertFailure "an+1 /= an + an-1"
