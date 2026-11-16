#!/bin/sh
# -----------------------------------------------------------------------------
#
# Copyright (c) 2017-2027 Sam Cox, Roberto Sommariva
#
# This file is part of the AtChem2 software package.
#
# This file is licensed under the MIT license, which can be found in the file
# `LICENSE` at the top level of the AtChem2 distribution.
#
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# This script downloads and installs openlibm into the Dependencies
# Directory, specified by input argument `$1`.
#
# Website: https://openlibm.org/
# Requirements: wget, gcc, make
#
# Usage:
#   ./install_openlibm.sh ~/path/to/dependencies/directory
# -----------------------------------------------------------------------------

OPENLIBM_VERSION="0.8.8"

# set path to the dependencies directory
if [ -z "$1" ] ; then
    printf "\n[openlibm] missing argument: path to dependencies directory\n"
    exit 1
else # convert to absolute path and check that directory exists
    DEP_DIR=$(cd "$1" 2>/dev/null && pwd)
    if [ -z "$DEP_DIR" ] ; then
        printf "\n[openlibm] %s does not exist\n" "$1"
        exit 1
    fi
    cd "$DEP_DIR" || exit 1
fi

# download archive
OPENLIBM_RELEASE="openlibm-${OPENLIBM_VERSION}"
OPENLIBM_ARCHIVE="v${OPENLIBM_VERSION}.tar.gz"
if ! wget "https://github.com/JuliaMath/openlibm/archive/${OPENLIBM_ARCHIVE}" ; then
    printf "\n[openlibm] wget --> FAIL\n"
    exit 1
fi

# unpack archive
if ! tar -zxf "$OPENLIBM_ARCHIVE" ; then
    printf "\n[openlibm] untar --> FAIL\n"
    exit 1
fi
rm -f "$OPENLIBM_ARCHIVE"

# compile and install
mv "${OPENLIBM_RELEASE}" openlibm && cd openlibm || exit 1
if ! make -j4 ; then
    printf "\n[openlibm] make --> FAIL\n"
    exit 1
fi

printf "\n[openlibm] version %s installed in %s\n\n" "$OPENLIBM_VERSION" "$DEP_DIR"
exit 0
