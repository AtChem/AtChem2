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

VERS_OLD="AtChem2 v1.2-dev"
VERS_NEW="AtChem2 v1.2"

find ./ -type f ! -name "version.sh" -print | xargs sed -i -e "s/$VERS_OLD/$VERS_NEW/g"
