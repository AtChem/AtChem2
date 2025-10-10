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
# This script downloads and installs CVODE (part of SUNDIALS) into the
# Dependencies Directory, specified by input argument `$1`.
#
# The default Fortran compiler is GNU gfortran. A different compiler
# can be specified by optional input argument `$2`.
#
# Website: https://computing.llnl.gov/projects/sundials/
# Requirements: gcc, fortran, cmake, make
#
# Usage:
#   ./install_cvode.sh ~/path/to/dependencies/directory
#     OR
#   ./install_cvode.sh ~/path/to/dependencies/directory /path/to/fortran/compiler
# -----------------------------------------------------------------------------

SUNDIALS_VERSION="2.7.0"

# path to dependencies directory
if [ -z "$1" ] ; then
    printf "\n[sundials] missing argument: path to dependencies directory\n"
    exit 1
else
    DEP_DIR="$1"
    if ! cd "$DEP_DIR"; then
        printf "\n[sundials] $DEP_DIR does not exist\n"
        exit 1
    fi
fi

# set fortran compiler (default: /usr/bin/gfortran)
if [ -z "$2" ] ; then
    if [ ! -f /usr/bin/gfortran ] ; then
        printf "\n[fortran] default compiler (/usr/bin/gfortran) does not exist\n"
        exit 1
    fi
    FORT_COMP=/usr/bin/gfortran
else
    FORT_COMP=$2
    if [ ! -f "$2" ] ; then
        printf "\n[fortran] selected compiler (%s) does not exist" "$2"
        exit 1
    fi
fi

# download archive
SUNDIALS_DIR="sundials-${SUNDIALS_VERSION}"
SUNDIALS_ARCHIVE="v${SUNDIALS_VERSION}.tar.gz"
wget "https://github.com/LLNL/sundials/archive/${SUNDIALS_ARCHIVE}"
if [ $? -ne 0 ] ; then
    printf "\n[sundials] wget --> FAIL\n"
    exit 1
fi

# unpack archive
tar -zxf "$SUNDIALS_ARCHIVE"
if [ $? -ne 0 ] ; then
    printf "\n[sundials] untar --> FAIL\n"
    exit 1
fi
rm -f "$SUNDIALS_ARCHIVE"

# compile and install CVODE
cd "$SUNDIALS_DIR"
CVODE_VERSION=$(grep cvodelib_VERSION CMakeLists.txt | cut -d '"' -f2)
mkdir build/ && cd build/
cmake -DCMAKE_INSTALL_PREFIX="$DEP_DIR/cvode" \
      -DCMAKE_C_COMPILER:FILEPATH=gcc \
      -DCMAKE_Fortran_COMPILER=$FORT_COMP \
      -DBUILD_ARKODE:BOOL=OFF \
      -DBUILD_CVODE:BOOL=ON \
      -DBUILD_CVODES:BOOL=OFF \
      -DBUILD_IDA:BOOL=OFF \
      -DBUILD_IDAS:BOOL=OFF \
      -DBUILD_KINSOL:BOOL=OFF \
      -DLAPACK_ENABLE:BOOL=OFF \
      -DFCMIX_ENABLE:BOOL=ON \
      -DEXAMPLES_ENABLE:BOOL=OFF \
      -DCMAKE_MACOSX_RPATH:BOOL=ON \
      ..
if [ $? -ne 0 ] ; then
    printf "\n[cvode] cmake --> FAIL\n"
    exit 1
fi

make -j4
if [ $? -ne 0 ] ; then
    printf "\n[cvode] make --> FAIL\n"
    exit 1
fi

make install
if [ $? -ne 0 ] ; then
    printf "\n[cvode] make install --> FAIL\n"
    exit 1
fi

# finish installation
printf "\n[cvode] version %s installed in %s\n" "$CVODE_VERSION" "$DEP_DIR"
exit 0
