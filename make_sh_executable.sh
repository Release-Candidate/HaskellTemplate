#!/bin/bash
# SPDX-License-Identifier: MIT
# Copyright (C) 2021 Roland Csaszar
#
# Project:  HaskellTemplate
# File:     make_template_file.sh
# Date:     18.May.2021
###############################################################################


# Add the executable bit to '.sh' shell scripts, as the template engine
# doesn't set them.

find  ./ -name "*.sh" -exec git add --chmod=+x {} \;
