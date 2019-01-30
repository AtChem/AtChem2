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
#    - mechanism.f90
#    By default, it is ./model/configuration/
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
echo "facsimile mechanism file:" $1
echo "auto-generated fortran files directory:" $2
echo "fortran mechanism directory:" $3
echo "mcm files directory:" $4

echo ''
echo "call mech_converter.py"
python ./tools/mech_converter.py $1 $2 $3 $4

echo ''
if [ -z $3 ]; then
  echo "make sharedlib"
  make sharedlib
else
  echo "make sharedlib" $3
  make sharedlib SHAREDLIBDIR=$3
fi
echo "make" $1
make
