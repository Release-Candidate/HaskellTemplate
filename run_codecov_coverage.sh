#!/bin/bash
# SPDX-License-Identifier: MIT
# Copyright (C) 2021 Roland Csaszar
#
# Project:  TestHaskell
# File:     run_codecov_coverage.sh
# Date:     18.May.2021
###############################################################################

HPC_ROOT_DIR=$(stack path --local-hpc-root)
TIX_PATH=$(find "${HPC_ROOT_DIR}"/TestHaskell -name "*.tix")
MIX_DIR=$(stack path --dist-dir)

stack clean
stack test --coverage
stack exec -- hpc-codecov --verbose -o coverage.json "${TIX_PATH}" -m "${MIX_DIR}/hpc"
