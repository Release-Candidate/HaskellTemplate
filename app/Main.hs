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

import Lib (fibN)
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

-- | Record to hold the command line arguments, `start` and `end`.
--
-- > fun start end = start * end
data CmdLine = CmdLine
  { -- | The start value of the calculation.
    start :: Int,
    -- | The end value of the calculation.
    end :: Int
  }
  deriving (Data, Typeable, Show, Eq)

-- | The program's command line parser definition.
cmdLine :: CmdLine
cmdLine =
  CmdLine
    { -- default value | placeholder | description
      start = 1 &= typ "START" &= help "Start of the line - START",
      end = 100 &= typ "END" &= help "End of the Line, END."
    }
    &= help "Calculate between START and END."
    &= summary "Version 0.0.1"
    &= details
      [ "Calculates the BLA between the values START and END",
        "",
        "Example:",
        "\tBLA --start 4 --end 69"
      ]

-- | Main entry point.
main :: IO ()
main =
  do
    let mode = cmdArgsMode cmdLine
    CmdLine {start, end} <- cmdArgsRun mode
    print (start, end)
