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
# Script to build and compile the AtChem2 model. The script accepts four
# arguments which are provided via script flags, as described below. Three of
# the arguments are optional and, if not specified, assume the default values.
#
# `--mechanism` is the chemical mechanism file in FACSIMILE (`.fac`) or
#    KPP (`.kpp`) format. This argument is not optional.
#    Default value: NONE.
#
# `--configuration` is the configuration directory, which contains
#    the model configuration files (`*.config` and `*.parameters`) and
#    the user-defined functions (`customRateFuncs.f90`).
#    Default value: ./model/configuration
#
# `--shared_lib` is the shared library directory, which contains
#    the chemical mechanism in Fortran format (`mechanism.*`) and
#    the pre-compiled mechanism shared library (`mechanism.so`).
#    Default value: ./model/configuration/include
#
# `--mcm` is the MCM version of the chemical mechanism, which sets the
#    reference list of organic peroxy radicals (RO2) and the empirical
#    parameters used to calculate the photolysis rates. The corresponding
#    data files (`peroxy-radicals_*` and `photolysis-rates_*`) are kept
#    in the `mcm/` directory.
#    Default value: `v3.3.1`
#
# NB: the script must be run from the *Main Directory* of AtChem2.
#
# Usage:
#   ./build/build_atchem2.sh --mechanism=/path/to/mechanism/file
#     OR
#   ./build/build_atchem2.sh --mechanism=/path/to/mechanism/file
#                            --configuration=/path/to/configuration/directory
#                            --shared_lib=/path/to/mechanism/directory
#                            --mcm=v3.3.1
# -----------------------------------------------------------------------------
set -eu

# default values for the script flags
MECHF=""
CONFIGD="model/configuration"
MECHD="model/configuration/include"
MCMV="v3.3.1"

# parse the script flags
while [ "$#" -gt 0 ]; do
    case "$1" in
        --mechanism=*)
            MECHF="${1#*=}"
            ;;
        --configuration=*)
            CONFIGD="${1#*=}"
            ;;
        --shared_lib=*)
            MECHD="${1#*=}"
            ;;
        --mcm=*)
            MCMV="${1#*=}"
            ;;
        *)
            printf "\n[INPUT ERROR] Invalid argument: %s\n" "$1" >&2
            exit 1
            ;;
    esac
    shift
done

# ============================================================ #

# set the chemical mechanism file (`--mechanism=`)
if [ -z "$MECHF" ] ; then
    printf "\n[INPUT ERROR] The chemical mechanism file (.fac/.kpp) must be provided.\n"
    exit 1
else
    if [ ! -f "$MECHF" ]; then
        printf "\n[INPUT ERROR] The chemical mechanism file does not exist.\n"
        exit 1
    else
        printf "\n[*] Chemical mechanism file: %s\n" "$MECHF"
    fi
fi

# set the model configuration directory (`--configuration=`)
printf "\n[*] Model configuration directory: %s\n" "$CONFIGD"
if [ ! -d "$CONFIGD" ]; then
    printf "\n[INPUT ERROR] The model configuration directory does not exist.\n"
    exit 1
fi

# set the shared library directory (`--shared_lib=`)
printf "\n[*] Shared library directory: %s\n" "$MECHD"
if [ ! -d "$MECHD" ]; then
    printf "\n[INPUT ERROR] The shared library directory does not exist.\n"
    exit 1
fi

# set the MCM version (`--mcm=`)
printf "\n[*] MCM version: %s\n" "$MCMV"
if [ "$MCMV" != "v3.3.1" ] && [ "$MCMV" != "v3.2" ] && [ "$MCMV" != "v3.1" ]; then
    printf "\n[INPUT ERROR] Invalid MCM version. Supported versions: v3.1, v3.2, v3.3.1\n"
    exit 1
fi

# ============================================================ #

# check python version
if command -v python3 >/dev/null 2>&1; then
    PY_BIN=python3
elif command -v python2 >/dev/null 2>&1; then
    PY_BIN=python2
else
    printf "\n[ERROR] Neither 'python3' nor 'python2' executable found in PATH.\n" >&2
    exit 1
fi
printf "\n--> Using %s\n" "$PY_BIN"

# compile the chemical mechanism shared library (`mechanism.so`)
printf "\n--> Compiling chemical mechanism shared library...\n\n"
make sharedlib PYTHON_BIN="$PY_BIN" MECHFILE="$MECHF" CONFIGDIR="$CONFIGD" MECHDIR="$MECHD" MCMVERS="$MCMV"
if [ $? -ne 0 ] ; then
    printf "\n[FAIL] Check error message for details.\n"
    exit 1
fi

# compile the atchem executable
printf "\n--> Compiling atchem2 executable...\n\n"
make -j
if [ $? -ne 0 ] ; then
    printf "\n[FAIL] Check error message for details.\n"
    exit 1
fi

printf "\n--> AtChem2 build process successfully completed!\n\n"
exit 0
