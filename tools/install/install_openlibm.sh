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
# This script downloads and installs openlibm into the Dependencies
# Directory, specified by input argument `$1`.
#
# Website: https://openlibm.org/
# Requirements: gcc, make
#
# Usage:
#   ./install_openlibm.sh ~/path/to/dependencies/directory
# -----------------------------------------------------------------------------

OPENLIBM_VERSION="0.8.7"

# path to dependencies directory
if [ -z "$1" ] ; then
    printf "\n[openlibm] missing argument: path to dependencies directory\n"
    exit 1
else
    DEP_DIR="$1"
    if ! cd "$DEP_DIR"; then
        printf "\n[openlibm] $DEP_DIR does not exist\n"
        exit 1
    fi
fi

# download archive
OPENLIBM_ARCHIVE="v${OPENLIBM_VERSION}.tar.gz"
wget "https://github.com/JuliaMath/openlibm/archive/${OPENLIBM_ARCHIVE}"
if [ $? -ne 0 ] ; then
    printf "\n[openlibm] wget --> FAIL\n"
    exit 1
fi

# unpack archive
tar -zxf "$OPENLIBM_ARCHIVE"
if [ $? -ne 0 ] ; then
    printf "\n[openlibm] untar --> FAIL\n"
    exit 1
fi
rm -f "$OPENLIBM_ARCHIVE"

# compile and install
mv "openlibm-${OPENLIBM_VERSION}" openlibm
cd openlibm
make -j4
if [ $? -ne 0 ] ; then
    printf "\n[openlibm] make --> FAIL\n"
    exit 1
fi

# finish installation
printf "\n[openlibm] version %s installed in %s\n" "$OPENLIBM_VERSION" "$DEP_DIR"
exit 0
