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
# Script to process the mechanism file (*.fac) and convert it to the
# correct format for AtChem2.
#
# N.B.: the script MUST be run from the base directory of AtChem2.
#
# $1 is the location of the chemical mechanism file in FACSIMILE
#    format. This argument is NOT optional, and there is no
#    default.
#
# $2 is the directory for the auto-generated Fortran files:
# TODO: delete this!
#    - mechanism-rate-coefficients.f90
#    By default, it is ./src/gen/
#
# $3 is the directory for the chemical mechanism in Fortran format:
#    - mechanism.prod
#    - mechanism.reac
#    - mechanism.species
#    By default, it is ./model/configuration/
#
# $4 is the directory for MCM-specific files:
#    - list of organic peroxy radicals
#    - parameters to calculate photolysis rates
#    By default, it is ./mcm/

set -e
echo ''
echo "facsimile mechanism directory:" $1
echo "auto-generated fortran files directory:" $2
echo "fortran mechanism directory:" $3
echo "mcm files directory:" $4

echo ''
echo "call setup_atchem2.py"
python ./tools/setup_atchem2.py $1 $2 $3 $4

echo ''
echo "make" $1
make --always-make
