:: SPDX-License-Identifier: MIT
:: Copyright (C) 2021 Roland Csaszar
::
:: Project:  TestHaskell
:: File:     build_tools.bat
:: Date:     13.May.2021
:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
@echo off
:: Locally install mkdocs
pipenv install --dev

:: Locally build the needed Haskell tools.
stack install hlint hoogle implicit-hie ghcid ormolu haskell-dap ghci-dap haskell-debug-adapter
:: stack build weeder
:: stack build phoityne-vscode

echo Don't forget to add the directory that is mentioned in 'Copied executables to'
echo to your PATH or use 'stack exec TOOL'

:: stack exec gen-hie > hie.yaml
