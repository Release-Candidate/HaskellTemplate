-- SPDX-License-Identifier: MIT
-- Copyright (C) 2021 Roland Csaszar
--
-- Project:  TestHaskell
-- File:     GoldenRatioSpec.hs
-- Date:     15.May.2021
--
--------------------------------------------------------------------------------

-- | Tests in this module should be automatically discovered by HSpec.
module GoldenRatioSpec (spec) where

import Lib (goldenRatio)
import qualified Test.Hspec as HS
import qualified Test.Hspec.Expectations.Pretty as P
import qualified Test.Hspec.Hedgehog as HH
import qualified Test.Hspec.LeanCheck as LC
import qualified Test.Hspec.SmallCheck as SC
import qualified Test.Hspec.Tables as T
import qualified Test.QuickCheck as QC

-- | Golden ratio is \( \frac{1 + \sqrt{5}}{2} \).
goldenRatioConst :: Double
goldenRatioConst = (1.0 + sqrt 5) / 2.0

-- | Define an epsilon with a given number of fractional digits of precision.
-- 15 is the biggest number that makes sense for the golden ratio.
epsilon :: Double -> Double
epsilon f = 10.0 ** (- f)

-- | Test the iterative calculation of the golden ratio using Fibonacci numbers
--  actually works ;).
spec :: HS.Spec
spec = do
  HS.describe "Golden Ratio" $ do
    HS.it "QuickCheck: get biggest epsilon that fails" $
      QC.property $
        \f -> abs (goldenRatioConst - goldenRatio 10) `P.shouldSatisfy` (< epsilon f)

    HS.it "SmallCheck: get biggest epsilon that fails" $
      SC.property $
        \f -> abs (goldenRatioConst - goldenRatio 10) `P.shouldSatisfy` (< epsilon f)

    HS.it "LeanCheck: get biggest epsilon that fails" $
      LC.property $
        \f -> abs (goldenRatioConst - goldenRatio 10) `P.shouldSatisfy` (< epsilon f)

    HS.it "Compare golden ratio against constant, 8 digits." $
      HS.example $ abs (goldenRatioConst - goldenRatio 8) `P.shouldSatisfy` (< epsilon 8.0)

    T.byExample
      ("Number of fractional digits", "Actual number of fra. digits")
      [ (3 :: Int, 3 :: Int),
        (4, 4),
        (5, 5),
        (6, 6),
        (7, 7),
        (8, 8),
        (9, 9),
        (10, 10),
        (11, 11),
        (12, 12),
        (13, 13),
        (14, 14),
        (15, 15)
      ]
      (\e f -> abs (goldenRatioConst - goldenRatio e) `P.shouldSatisfy` (< epsilon (fromIntegral f)))
