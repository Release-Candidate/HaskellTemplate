#!/bin/bash
# SPDX-License-Identifier: MIT
# Copyright (C) 2021 Roland Csaszar
#
# Project:  TestHaskell
# File:     run_codecov_coverage.sh
# Date:     18.May.2021
###############################################################################

TIX_DIR=$(stack path --local-hpc-root)
MIX_DIR=$(stack path --dist-dir)

stack clean
stack test --coverage
stack exec -- hpc-codecov --verbose -o coverage.json "${TIX_DIR}\TestHaskell\TestHaskell-test\TestHaskell-test.tix" -m "${MIX_DIR}\hpc"
