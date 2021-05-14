-- SPDX-License-Identifier: MIT
-- Copyright (C) 2021 Roland Csaszar
--
-- Project:  TestHaskell
-- File:     Lib.hs
-- Date:     12.May.2021
--
--------------------------------------------------------------------------------

module Lib (fibN, fibBetter) where

-- | Calculate a list of Fibonacci numbers of length `len`.
fibN :: (Num a, Eq a, Enum a) => a -> [a]
fibN len = [fibNaive x | x <- [7 .. len]]

-- | Correct, but slow implementation.
fibNaive :: (Eq a, Num a, Num p) => a -> p
fibNaive 0 = 1
fibNaive 1 = 1
fibNaive len = fibNaive (len - 1) + fibNaive (len - 2)

-- | Better version, recursively construct a list.
fibBetter :: Integer -> [Integer]
fibBetter = fibHelper []
  where
    fibHelper :: [Integer] -> Integer -> [Integer]
    fibHelper list 0 = list
    fibHelper list n =
      case drop (length list - 2) list of
        [x, y] -> fibHelper (list ++ [x + y]) (n - 1)
        [] -> fibHelper [1] (n - 1)
        [1] -> fibHelper [1, 1] (n - 1)
        _ -> []
