#!/bin/bash
# SPDX-License-Identifier: MIT
# Copyright (C) 2021 Roland Csaszar
#
# Project:  TestHaskell
# File:     install_build_tools.sh
# Date:     18.May.2021
###############################################################################

# Locally install mkdocs
pipenv install --dev

# Locally build the needed Haskell tools.
stack build --copy-compiler-tool hlint
stack build --copy-compiler-tool hoogle
stack build --copy-compiler-tool implicit-hie
stack build --copy-compiler-tool ghcid
stack build --copy-compiler-tool haskell-dap
stack build --copy-compiler-tool ghci-dap
stack build --copy-compiler-tool haskell-debug-adapter
stack build --copy-compiler-tool hoogle
stack build --copy-compiler-tool ormolu
stack build --copy-compiler-tool weeder
# stack build --copy-compiler-tool phoityne-vscode

echo "Don't forget to add the directory that is mentioned in 'Copied executables to'"
echo "to your PATH or use 'stack exec TOOL'"

stack exec gen-hie > hie.yaml
