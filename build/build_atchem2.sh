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
# Script to build and compile AtChem2.
#
# $1 is the chemical mechanism file in FACSIMILE or KPP format.
#    Argument $1 is NOT optional, and there is no default.
#
# $2 is the comfiguration directory for the chemical mechanism in
#     Fortran format (mechanism.f90, mechanism.species,
#     mechanism.reac, mechanism.prod, mechanism.ro2) and the chemical
#     mechanism shared library (mechanism.so).
#     By default, argument $2 is: ./model/configuration/
#
# $3 is the directory of the MCM data files, which contains:
#    - list of organic peroxy radicals (RO2)
#    - parameters to calculate photolysis rates
#    By default, argument $3 is: ./mcm/
#
# Usage:
#   ./build/build_atchem2.sh /path/to/mechanism/file
#   OR
#   ./build/build_atchem2.sh /path/to/mechanism/file /path/to/mechanism/directory
# -----------------------------------------------------------------------------
set -eu

printf "\n--> Building AtChem2...\n"

# set chemical mechanism file (argument $1)
if [ -z "$1" ] ; then
    printf "\n[INPUT ERROR] Missing argument: chemical mechanism file (.fac/.kpp).\n"
    exit 1
else
    MECHF="$1"
    if [ ! -f "$MECHF" ]; then
        printf "\n[INPUT ERROR] The chemical mechanism file does not exist.\n"
        exit 1
    else
        printf "\n[*] Chemical mechanism file: %s\n" "$MECHF"
    fi
fi

# set model configuration directory (argument $2)
CONFIGD="${2:-./model/configuration/}"
printf "\n[*] Configuration directory: %s\n" "$CONFIGD"
if [ ! -d "$CONFIGD" ]; then
    printf "\n[INPUT ERROR] The configuration directory does not exist.\n"
    exit 1
fi

# set MCM data directory (argument $3)
MCMV="${3:-./mcm/}"
printf "\n[*] MCM data directory: %s\n" "$MCMV"
if [ ! -d "$MCMV" ]; then
    printf "\n[INPUT ERROR] The MCM data directory does not exist.\n"
    exit 1
fi

# compile chemical mechanism shared library
printf "\n--> Compiling shared library...\n"
make sharedlib MECHFILE="$MECHF" CONFIGDIR="$CONFIGD" MCMDIR="$MCMV"
if [ $? -ne 0 ] ; then
    printf "\n[FAIL] Check output above for details.\n"
    exit 1
fi

# compile atchem executable
printf "\n--> Compiling atchem executable...\n"
make -j
if [ $? -ne 0 ] ; then
    printf "\n[FAIL] Check output above for details.\n"
    exit 1
fi

printf "\n--> AtChem2 build process successfully completed!\n"
exit 0
