-- SPDX-License-Identifier: MIT
-- Copyright (C) 2021 Roland Csaszar
--
-- Project:  TestHaskell
-- File:     Lib.hs
-- Date:     12.May.2021
--
--------------------------------------------------------------------------------

module Lib
  ( someFunc,
    anotherFunc,
  )
where

-- | Test documentation
someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- | Documentation too.
-- ### Params:
--         x - the x coordinate.
--         y - the y coordinate.
anotherFunc :: Num a => a -> a -> a
anotherFunc x y = x * y
