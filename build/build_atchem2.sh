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
#    - mechanism.f90
#    - mechanism.species
#    - mechanism.reac
#    - mechanism.prod
#    - mechanism.ro2

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

printf "\n     AtChem2 v1.3-dev\n"
printf "\n* Chemical mechanism file: %s\n" $1
printf "\n* Model configuration directory [ default = ./model/configuration/ ] %s\n:" $2
printf "\n* MCM data files directory [ default = ./mcm/ ] %s\n:" $3

printf "\n-> Create shared library\n"
if [ -z $2 ]; then
    make sharedlib
    printf "\n=> fortran mechanism created in: ./model/configuration/\n"
    printf "\n=> shared library created in: ./model/\n"
else
    make sharedlib MECHFILE=$1 CONFIGDIR=$2
    printf "\n=> fortran mechanism created in %s\n:" $2
    printf "\n=> shared library created in %s\n:" $1
fi

printf "\n-> Create atchem2 executable\n"
make

exit 0
