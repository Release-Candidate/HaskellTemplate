#!/bin/bash
# SPDX-License-Identifier: MIT
# Copyright (C) 2021 Roland Csaszar
#
# Project:  HaskellTemplate
# File:     make_template_file.sh
# Date:     18.May.2021
###############################################################################

# Name of the template file to generate.
TEMPLATE_FILE="BigTemplate.hsfiles"

# Totally ignore this file.
IGNORE_ALL="make_template.sh"

# Strings to substitue with placeholders.
PROJECT_NAME="TestHaskell"
PROJECT_PH="{{name}}"

GITUSER_NAME="Release-Candidate"
GITUSER_PH="{{github-username}}"

AUTHOR_NAME="Roland Csaszar"
AUTHOR_PH="{{author-name}}"

EMAIL_NAME="rec@gmx.at"
EMAIL_PH="{{author-email}}"

COPYRIGHT_NAME="2021 Roland Csaszar"
COPYRIGHT_PH="{{copyright}}"

# All files under Git control, withoud everything in ` docs/html` and the
# template file itself.
FILE_LIST=$(git ls-files | grep -v "docs/html" | grep -v ".hsfiles" | grep -v ".github/workflows/")

if [ -e "${TEMPLATE_FILE}" ]; then
    rm "${TEMPLATE_FILE}"
fi

for FILE_NAME in ${FILE_LIST}; do
    FILE_HEADER="{-# START_FILE ${FILE_NAME} #-}"

    {
    echo "${FILE_HEADER}"

    if [ "${FILE_NAME}" == "${IGNORE_ALL}" ]; then
        cat "${FILE_NAME}"

    elif [ "$(echo "${FILE_NAME}" | grep -c ".*\.hs$" )" == "1" ]; then
        sed 's/'${PROJECT_NAME}'/'${PROJECT_PH}'/g;s/'${GITUSER_NAME}'/'${GITUSER_PH}'/g;s/'${EMAIL_NAME}'/'${EMAIL_PH}'/g' "${FILE_NAME}" \
        | sed 's/ Date:     [0-9]*\.\S*\.20[0-9][0-9]/ Date:     '"$(date +"%d.%m.%Y")"'/'

    elif [ "$(echo "${FILE_NAME}" | grep -c ".*\.sh$" )" == "1" ]; then
        sed 's/'${PROJECT_NAME}'/'${PROJECT_PH}'/g;s/'${GITUSER_NAME}'/'${GITUSER_PH}'/g;s/'${EMAIL_NAME}'/'${EMAIL_PH}'/g' "${FILE_NAME}" \
        | sed 's/ Date:     [0-9]*\.\S*\.20[0-9][0-9]/ Date:     '"$(date +"%d.%m.%Y")"'/'

     elif [ "$(echo "${FILE_NAME}" | grep -c ".*\.bat$" )" == "1" ]; then
        sed 's/'${PROJECT_NAME}'/'${PROJECT_PH}'/g;s/'${GITUSER_NAME}'/'${GITUSER_PH}'/g;s/'${EMAIL_NAME}'/'${EMAIL_PH}'/g' "${FILE_NAME}" \
        | sed 's/ Date:     [0-9]*\.\S*\.20[0-9][0-9]/ Date:     '"$(date +"%d.%m.%Y")"'/'

    else
        sed 's/'${PROJECT_NAME}'/'${PROJECT_PH}'/g;s/'${GITUSER_NAME}'/'${GITUSER_PH}'/g;s/'"${AUTHOR_NAME}"'/'${AUTHOR_PH}'/g;s/'${EMAIL_NAME}'/'${EMAIL_PH}'/g;s/'"${COPYRIGHT_NAME}"'/'${COPYRIGHT_PH}'/g' "${FILE_NAME}" \
        | sed 's/ Date:     [0-9]*\.\S*\.20[0-9][0-9]/ Date:     '"$(date +"%d.%m.%Y")"'/'
    fi
    echo ""
    } >> "${TEMPLATE_FILE}"

done