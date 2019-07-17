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

#!/bin/sh
#
# Script to update the version number of Atchem2.
#
# N.B.: the script MUST be run from the base directory of AtChem2.

VERS_OLD="v1.2-dev"
VERS_NEW="v1.2"

find ./ -type f ! -name "version.sh" -print | xargs perl -pi -e "s/$VERS_OLD/$VERS_NEW/g"

echo "AtChem2 version number changed to:" $VERS_NEW
