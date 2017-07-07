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
# $1 is the mechanism input file
#
# $2 is the directory in which to place the output files:
#    - mechanism-rate-coefficients.f90
#    - mechanism-rate-declarations.f90
#    By default, it is the base directory of AtChem2.
#
# $3 is the directory in which to place the output files:
#    - mechanism.prod
#    - mechanism.reac
#    - mechanism.species
#    By default it is modelConfiguration/ in the base directory of AtChem2.

set -e
echo ''
echo "input mechanism file:" $1
echo "fortran output directory:" $2
echo "mechanism output directory:" $3

echo ''
echo "call setup_atchem2.py"
python ./tools/setup_atchem2.py $1 $2 $3

echo ''
echo "make" $1
make --always-make
