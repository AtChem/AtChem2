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

# ------------------------------------------------------------------ #
# Script to change the version number of AtChem2.
#
# NB: the script must be run from the Main Directory of AtChem2.
# ------------------------------------------------------------------ #
set -eu

VERS_OLD="v1.2.3"
VERS_NEW="v1.3-dev"

# ignore the .git/ directory, exclude this script and the changelog file
find ./ -not -path "./.git/*" -type f \
     ! -name "update_version_number.sh" \
     ! -name "CHANGELOG.md" \
     -print0 | xargs -0 perl -pi -e "s/$VERS_OLD/$VERS_NEW/g"

printf "\n--> AtChem2 version number changed to: %s\n" "$VERS_NEW"
exit 0
