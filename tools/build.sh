#!/bin/sh

# $1 is the input file
# $2 is the directory in which to place the mechanism-rate-coefficients.f90 and
#    mechanism-rate-declarations.f90 files. This should point to the base directory
#    of the source files of AtChem.
set -e
echo ''
echo $1
echo $2
echo $3
echo "call ./tools/setup_atchem.py"
python ./tools/setup_atchem.py $1 $2 $3
echo ''
echo "make" $1
make --always-make
