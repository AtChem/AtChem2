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

# Script to process the mechanism file (*.fac) and convert it to the
# correct format for AtChem2.
#
# N.B.: the script MUST be run from the base directory of AtChem2.
#
# $1 is the location of the chemical mechanism file in FACSIMILE
#    format. This argument is NOT optional, and there is no
#    default.
#
# $2 is the directory for the chemical mechanism in Fortran format:
#    - mechanism.species
#    - mechanism.reac
#    - mechanism.prod
#    - mechanism.ro2
#    - mechanism.f90
#    By default, it is ./model/configuration/
#
# $3 is the directory of the MCM data files:
#    - list of organic peroxy radicals (RO2)
#    - parameters to calculate photolysis rates
#    By default, it is ./mcm/

set -e
echo ""
echo "* facsimile mechanism file:" $1
echo "* fortran mechanism directory [ default = ./model/configuration/ ]:" $2
echo "* mcm data files directory [ default = ./mcm/ ]:" $3

echo ""
echo "call mech_converter.py"
python ./build/mech_converter.py $1 $2 $3

echo ""
if [ -z $2 ]; then
  echo "make sharedlib"
  make sharedlib
  echo "=> shared library directory: ./model/configuration/"
else
  echo "make sharedlib" $2
  make sharedlib SHAREDLIBDIR=$2
  echo "=> shared library directory:" $2
fi

echo ""
echo "make base executable"
make

exit 0
