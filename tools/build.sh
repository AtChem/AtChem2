#!/bin/sh
#
# Script to process the mechanism file (*.fac) and convert it to the
# correct format for AtChem.
#
# N.B.: the script MUST be run from the base directory of AtChem.
#
# $1 is the mechanism input file
#
# $2 is the directory in which to place the output files:
#    - mechanism-rate-coefficients.f90
#    - mechanism-rate-declarations.f90
#    By default, it is the base directory of AtChem.
#
# $3 is the directory in which to place the output files:
#    - mechanism.prod
#    - mechanism.reac
#    - mechanism.species
#    By default it is modelConfiguration/ in the base directory of AtChem.

set -e
echo ''
echo "input mechanism file:" $1
echo "fortran output directory:" $2
echo "mechanism output directory:" $3

echo ''
echo "call setup_atchem.py"
python ./tools/setup_atchem.py $1 $2 $3

echo ''
echo "make" $1
make --always-make
