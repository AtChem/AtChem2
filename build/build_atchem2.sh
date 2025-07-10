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

# -----------------------------------------------------------------------------
# Script to process the mechanism file (*.fac) and convert it to the
# correct format for AtChem2.
#
# $1 is the location of the chemical mechanism file in FACSIMILE
#    format. Argument $1 is NOT optional, and there is no default.
#
# $2 is the directory for the chemical mechanism in Fortran format:
#    - mechanism.species
#    - mechanism.reac
#    - mechanism.prod
#    - mechanism.ro2
#    - mechanism.f90
#    By default, argument $2 is: ./model/configuration/
#
# $3 is the directory of the MCM data files:
#    - list of organic peroxy radicals (RO2)
#    - parameters to calculate photolysis rates
#    By default, argument $3 is: ./mcm/
#
# Usage:
#   ./build/build_atchem2.sh /path/to/mechanism/file
#   ./build/build_atchem2.sh /path/to/mechanism/file /path/to/mechanism/directory
# -----------------------------------------------------------------------------

echo ""
echo "     AtChem2 v1.3-dev"
echo ""

echo "* Chemical mechanism file:" $1
echo "* Fortran mechanism directory [ default = ./model/configuration/ ]:" $2
echo "* MCM data files directory [ default = ./mcm/ ]:" $3

echo ""
echo "-> Call mech_converter.py"
python ./build/mech_converter.py $1 $2 $3

echo ""
echo "-> Create shared library"
make sharedlib
if [ -z $2 ]; then
  echo "=> shared library created in : ./model/configuration/"
else
  echo "=> shared library created in :" $2
fi

echo ""
echo "-> Create atchem2 executable"
make
echo ""

exit 0
