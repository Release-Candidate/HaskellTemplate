#!/bin/bash
# SPDX-License-Identifier: MIT
# Copyright (C) 2021 Roland Csaszar
#
# Project:  TestHaskell
# File:     run_code.sh
# Date:     18.May.2021
###############################################################################
# Run VS Code or Code Insiders

# Code:
# stack exec code

# Code Insiders:
stack exec code-insiders TestHaskell.code-workspace
