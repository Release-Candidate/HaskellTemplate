-- SPDX-License-Identifier: MIT
-- Copyright (C) 2021 Roland Csaszar
--
-- Project:  TestHaskell
-- File:     Main.hs
-- Date:     12.May.2021
--
--------------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable #-}

-- | Module containing the program's main entry point.
module Main where

import qualified Lib
import System.Console.CmdArgs
  ( Data,
    Typeable,
    cmdArgsMode,
    cmdArgsRun,
    details,
    help,
    summary,
    typ,
    (&=),
  )
import Text.Printf (printf)

-- | Record to hold the command line arguments, `fibonacci` and `goldenRatio`.
data CmdLine = CmdLine
  { -- | The number of elements in the Fibonacci list to generate.
    fibonacci :: Int,
    -- | The length to apply the golden ratio to.
    goldenRatio :: Double
  }
  deriving (Data, Typeable, Show, Eq)

-- | The program's command line parser definition.
cmdLine :: CmdLine
cmdLine =
  CmdLine
    { -- default value | placeholder | description
      fibonacci = 50 &= typ "LENGTH" &= help "The number of elements in the list of Fibonacci numbers",
      goldenRatio = 4 &= typ "LENGTH" &= help "The length to apply the golden ratio to."
    }
    &= help "Calculates the Fibonacci numbers and the golden ratio to a given length."
    &= summary "Version 0.0.1"
    &= details
      [ "Calculates the Fibonacci numbers and the golden ratio to a given length.",
        "",
        "Example:",
        "\tTestHaskell --fibonacci=35 --goldenratio=7"
      ]

-- | Main entry point.
main :: IO ()
main =
  do
    let mode = cmdArgsMode cmdLine
    CmdLine {fibonacci, goldenRatio} <- cmdArgsRun mode
    printf "\n"
    printf "The first %v Fibonacci numbers are: \n" fibonacci
    print $ Lib.fibZip fibonacci
    printf "\n"
    printf "The golden ratio to the length of %v is: " goldenRatio
    print $ goldenRatio * Lib.goldenRatio 8
