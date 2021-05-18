#!/bin/bash
# SPDX-License-Identifier: MIT
# Copyright (C) 2021 Roland Csaszar
#
# Project:  TestHaskell
# File:     run_watch.sh
# Date:     18.May.2021
###############################################################################
# Watches for file changes to rebuild and rerun the tests.
stack test --fast --haddock-deps --file-watch
