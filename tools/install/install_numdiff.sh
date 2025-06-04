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
# This script downloads and installs numdiff into the Dependencies
# Directory, specified by input argument `$1`.
#
# Website: https://www.nongnu.org/numdiff/
# Requirements: gcc, make
#
# NB: after running the script, ensure that the path to the numdiff
#     executable is set in ~/.bash_profile (or ~/.profile):
#       PATH=$PATH:$HOME/../numdiff/bin
#
# Usage:
#   ./install_numdiff.sh ~/path/to/dependencies/directory
# -----------------------------------------------------------------------------

NUMDIFF_VERSION="5.9.0"

# path to dependencies directory
if [ -z "$1" ] ; then
    printf "\n[numdiff] missing argument: path to dependencies directory\n"
    exit 1
else
    DEP_DIR="$1"
    if ! cd "$DEP_DIR"; then
        printf "\n[numdiff] $DEP_DIR does not exist\n"
        exit 1
    fi
fi

# download archive
NUMDIFF_DIR="numdiff-${NUMDIFF_VERSION}"
NUMDIFF_ARCHIVE="${NUMDIFF_DIR}.tar.gz"
wget "https://savannah.nongnu.org/download/numdiff/${NUMDIFF_ARCHIVE}"
if [ $? -ne 0 ] ; then
    printf "\n[numdiff] wget --> FAIL\n"
    exit 1
fi

# unpack archive
tar -zxf "$NUMDIFF_ARCHIVE"
if [ $? -ne 0 ] ; then
    printf "\n[numdiff] untar --> FAIL\n"
    exit 1
fi
rm -f "$NUMDIFF_ARCHIVE"

# compile and install
cd "${NUMDIFF_DIR}"
if [ "$(uname -s)" = 'Darwin' ]; then
    ./configure --prefix=$1/numdiff CPPFLAGS=-I/usr/local/Cellar/gettext/0.20.1/include/ LDFLAGS=-L/usr/local/Cellar/gettext/0.20.1/lib
else
    ./configure --prefix=$1/numdiff
fi
if [ $? -ne 0 ] ; then
    printf "\n[numdiff] configure --> FAIL\n"
    exit 1
fi

make -j4
if [ $? -ne 0 ] ; then
    printf "\n[numdiff] make --> FAIL\n"
    exit 1
fi

make install
if [ $? -ne 0 ] ; then
    printf "\n[numdiff] make install --> FAIL\n"
    exit 1
fi

# finish installation
printf "\n[numdiff] version %s installed in %s\n" "$NUMDIFF_VERSION" "$DEP_DIR"
exit 0
