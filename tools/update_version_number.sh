#!/bin/sh
# -----------------------------------------------------------------------------
#
# Copyright (c) 2017-2027 Sam Cox, Roberto Sommariva
#
# This file is part of the AtChem2 software package.
#
# This file is licensed under the MIT license, which can be found in the file
# `LICENSE` at the top level of the AtChem2 distribution.
#
# -----------------------------------------------------------------------------

# ------------------------------------------------------------------ #
# Script to change the version number of AtChem2.
#
# NB: the script must be run from the *Main Directory* of AtChem2.
# ------------------------------------------------------------------ #
set -eu

# check that the script is run from the main directory
if [ ! -f tools/update_version_number.sh ]; then
    printf "\n[ERROR] Run this script from the AtChem2 Main Directory.\n" >&2
    exit 1
fi

# ================== ATCHEM2 VERSION NUMBER ================== #

VERS_OLD="v1.3"
VERS_NEW="v1.3.1-dev"

export VERS_OLD VERS_NEW

# ============================================================ #

# change the version number in the source files; ignore the .git/ directory,
# binaries, this script (update_version_number.sh) and the changelog file
find . -path ./.git -prune -o -type f \
     ! -name "update_version_number.sh" \
     ! -name "CHANGELOG.md" \
     -exec grep -lIF --null -- "$VERS_OLD" {} + |
    xargs -0 -r perl -pi -e 's/\Q$ENV{VERS_OLD}\E/$ENV{VERS_NEW}/g'

printf "\n--> AtChem2 version number changed to: %s\n" "$VERS_NEW"
exit 0
