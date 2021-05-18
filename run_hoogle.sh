#!/bin/bash
# SPDX-License-Identifier: MIT
# Copyright (C) 2021 Roland Csaszar
#
# Project:  TestHaskell
# File:     run_hoogle.sh
# Date:     18.May.2021
###############################################################################

# indexes the project's dependencies and displays the search.

stack hoogle -- generate --local
stack hoogle -- server --local --port=8080
