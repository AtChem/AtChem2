#!/bin/sh
# -----------------------------------------------------------------------------
#
# Copyright (c) 2017 Sam Cox, Roberto Sommariva
#
# This file is part of the AtChem2 software package.
#
# This file is covered by the MIT license which can be found in the file
# LICENSE.md at the top level of the AtChem2 distribution.
#
# -----------------------------------------------------------------------------

# Script to change the version number of AtChem2.
#
# N.B.: the script MUST be run from the main directory of AtChem2.

VERS_OLD="v1.2.2"
VERS_NEW="v1.3-dev"

# ignore the .git/ directory, exclude this script and the changelog file
find ./ -not -path "./.git/*" -type f ! -name "update_version_number.sh" ! -name "CHANGELOG.md" -print | xargs perl -pi -e "s/$VERS_OLD/$VERS_NEW/g"

echo "==> AtChem2 version number changed to:" $VERS_NEW
echo ""

exit 0
