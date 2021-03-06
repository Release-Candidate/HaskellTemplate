-- SPDX-License-Identifier: MIT
-- Copyright (C) 2021 Roland Csaszar
--
-- Project:  TestHaskell
-- File:     Lib.hs
-- Date:     12.May.2021
--
--------------------------------------------------------------------------------

module Lib (fibN, fibBetter, fibZip, goldenRatio) where

-- | Calculate a list of Fibonacci numbers of length `len`.
--
-- Example:
-- >>> fibN 9
-- [1,1,2,3,5,8,13,21,34]
fibN :: (Num a, Enum a, Ord a) => a -> [a]
fibN len
  | len < 1 = []
  | len == 1 = [1]
  | otherwise = [fibNaive x | x <- [0 .. len - 1]]

-- | Correct, but slow implementation.
--
-- Example:
-- >>> fibNaive 7
-- 21
fibNaive :: (Eq a, Num a, Num p) => a -> p
fibNaive 0 = 1
fibNaive 1 = 1
fibNaive len = fibNaive (len - 1) + fibNaive (len - 2)

-- | Better version, recursively construct a list.
--
-- Example:
-- >>> fibBetter 8
-- [1,1,2,3,5,8,13,21]
fibBetter :: Integer -> [Integer]
fibBetter len
  | len < 1 = []
  | len == 1 = [1]
  | otherwise = fibHelper [] len
  where
    fibHelper :: [Integer] -> Integer -> [Integer]
    fibHelper list 0 = list
    fibHelper list n =
      case drop (length list - 2) list of
        [x, y] -> fibHelper (list ++ [x + y]) (n - 1)
        [] -> fibHelper [1] (n - 1)
        [1] -> fibHelper [1, 1] (n - 1)
        _ -> []

-- | Short version, using zip with the same list twice.
--
-- Example:
-- >>> fibZip 10
-- [1,1,2,3,5,8,13,21,34,55]
fibZip :: Int -> [Integer]
fibZip len = take len fibZips
  where
    fibZips :: [Integer]
    fibZips = 1 : 1 : zipWith (+) fibZips (tail fibZips)

-- | Calculate the golden ratio.
--
-- Example:
-- >>> goldenRatio 4
-- 1.6180555555555556
goldenRatio :: Int -> Double
goldenRatio numDig = goldenHelper numDig 3
  where
    fibs = fibZip (maxBound :: Int)
    goldenHelper :: Int -> Int -> Double
    goldenHelper numdig l =
      case reverse $ take l fibs of
        [] -> 0.0
        [_] -> 1.0
        [_, _] -> 1.0
        x : y : z : _ ->
          if abs (curr - before) < epsilon
            then curr
            else goldenHelper numdig (l + 1)
          where
            curr :: Double = fromInteger x / fromInteger y
            before :: Double = fromInteger y / fromInteger z
            epsilon = 10.0 ** (-fromIntegral numdig :: Double)
