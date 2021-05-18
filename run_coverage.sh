#!/bin/bash
# SPDX-License-Identifier: MIT
# Copyright (C) 2021 Roland Csaszar
#
# Project:  TestHaskell
# File:     run_coverage.sh
# Date:     18.May.2021
###############################################################################

TIX_DIR=$(stack path --local-hpc-root)

@echo on
stack clean
stack test --coverage
stack exec -- hpc-lcov --file "${TIX_DIR}\TestHaskell\TestHaskell-test\TestHaskell-test.tix"
