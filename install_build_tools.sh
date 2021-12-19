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
stack install hlint hoogle implicit-hie ghcid ormolu haskell-dap ghci-dap haskell-debug-adapter hspec-discover

echo "Don't forget to add the directory that is mentioned in 'Copied executables to'"
echo "to your PATH or use 'stack exec TOOL'"

#stack exec gen-hie > hie.yaml
