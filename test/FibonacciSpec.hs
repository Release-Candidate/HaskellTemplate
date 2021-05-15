-- SPDX-License-Identifier: MIT
-- Copyright (C) 2021 Roland Csaszar
--
-- Project:  TestHaskell
-- File:     FibonacciSpec.hs
-- Date:     15.May.2021
--
--------------------------------------------------------------------------------

-- | This tests should be automatically discovered by HSpec.
module FibonacciSpec (spec) where

import Lib (fibBetter, fibN, fibZip)
import qualified Test.Hspec as HS
import qualified Test.Hspec.Expectations.Pretty as P
import qualified Test.Hspec.Hedgehog as HH
import qualified Test.Hspec.LeanCheck as LC
import qualified Test.Hspec.SmallCheck as SC
import qualified Test.Hspec.Tables as T
import qualified Test.QuickCheck as QC

-- | Test the generation of a list with Fibonacci numbers.
spec :: HS.Spec
spec = do
  HS.describe "Fibonacci" $ do
    HS.it "Must throw exception: not a Fibonacci series!" $
      checkFibonacci [1 .. 100] `HS.shouldThrow` HS.anyException

    T.byExample
      ("Length", "Naive Fibonacci list")
      [ (0, []),
        (1, [1]),
        (2, [1, 1]),
        (3, [1, 1, 2]),
        (4, [1, 1, 2, 3])
      ]
      (\(n :: Integer) result -> fibN n `P.shouldBe` result)

    T.byExample
      ("Length", "Better Fibonacci list")
      [ (0, []),
        (1, [1]),
        (2, [1, 1]),
        (3, [1, 1, 2]),
        (4, [1, 1, 2, 3])
      ]
      (\(n :: Integer) result -> fibBetter n `P.shouldBe` result)

    T.byExample
      ("Length", "Zip Fibonacci list")
      [ (0, []),
        (1, [1]),
        (2, [1, 1]),
        (3, [1, 1, 2]),
        (4, [1, 1, 2, 3])
      ]
      (\(n :: Int) result -> fibZip n `P.shouldBe` result)

    HS.it "Naive version: Check if an+1 = an + an-1, n < 37" $
      checkFibonacci $ fibN 38

    HS.it "Better version: Check if an+1 = an + an-1, n < 1000 " $
      checkFibonacci $ fibBetter 1000

    HS.it "Zip version: Check if an+1 = an + an-1, n < 1000 " $
      checkFibonacci $ fibZip 1000

    HS.it "Better version: Check using QuickCheck" $
      QC.property $
        \n -> do
          print n
          checkFibonacci $ fibBetter n

    HS.it "Better version: Check using SmallCheck" $
      SC.property $
        \n -> do
          print n
          checkFibonacci $ fibBetter n

    HS.it "Zip version: Check using SmallCheck" $
      SC.property $
        \n -> do
          print n
          checkFibonacci $ fibZip n

    HS.it "Zip version: Check using LeanCheck" $
      LC.property $
        \n -> do
          print n
          checkFibonacci $ fibZip n

-- | Test, if for the given list `an+1 = an + an-1` holds for all elements.
checkFibonacci :: [Integer] -> P.Expectation
checkFibonacci list =
  case list of
    [] -> list `P.shouldBe` []
    [_] -> list `P.shouldBe` [1 :: Integer]
    [_, _] -> True `P.shouldBe` True
    x : y : z : xs
      | z == x + y -> checkFibonacci (y : z : xs)
      | otherwise -> x + y `P.shouldBe` z
