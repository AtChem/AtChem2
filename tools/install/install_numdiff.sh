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
# This script downloads and installs numdiff into the Dependencies
# Directory, specified by input argument `$1`.
#
# Website: https://www.nongnu.org/numdiff/
# Requirements: wget, gcc, make
#
# AFTER running the script, ensure that the numdiff binary path is set by adding
# the following line to `~/.bash_profile` (or `~/.profile`):
#   export PATH=$PATH:$HOME/path/to/dependencies/directory/numdiff/bin
#
# Usage:
#   ./install_numdiff.sh ~/path/to/dependencies/directory
# -----------------------------------------------------------------------------

NUMDIFF_VERSION="5.9.0"

# set path to the dependencies directory
if [ -z "$1" ] ; then
    printf "\n[numdiff] missing argument: path to dependencies directory\n"
    exit 1
else # convert to absolute path and check that directory exists
    DEP_DIR=$(cd "$1" 2>/dev/null && pwd)
    if [ -z "$DEP_DIR" ] ; then
        printf "\n[numdiff] %s does not exist\n" "$1"
        exit 1
    fi
    cd "$DEP_DIR" || exit 1
fi

# download archive
NUMDIFF_RELEASE="numdiff-${NUMDIFF_VERSION}"
NUMDIFF_ARCHIVE="${NUMDIFF_RELEASE}.tar.gz"
if ! wget "https://download.savannah.gnu.org/releases/numdiff/${NUMDIFF_ARCHIVE}" ; then
    printf "\n[numdiff] wget --> FAIL\n"
    exit 1
fi

# unpack archive
if ! tar -zxf "$NUMDIFF_ARCHIVE" ; then
    printf "\n[numdiff] untar --> FAIL\n"
    exit 1
fi
rm -f "$NUMDIFF_ARCHIVE"

# compile and install
cd "${NUMDIFF_RELEASE}" || exit 1
if [ "$(uname -s)" = 'Darwin' ]; then
    ./configure --prefix="$DEP_DIR/numdiff" CPPFLAGS=-I/usr/local/Cellar/gettext/0.20.1/include/ LDFLAGS=-L/usr/local/Cellar/gettext/0.20.1/lib
else
    ./configure --prefix="$DEP_DIR/numdiff"
fi
if [ $? -ne 0 ] ; then
    printf "\n[numdiff] configure --> FAIL\n"
    exit 1
fi

if ! make -j4 CFLAGS="-g -O2 -std=gnu11" ; then # force newer compilers to use C11 standard 
    printf "\n[numdiff] make --> FAIL\n"
    exit 1
fi

if ! make install ; then
    printf "\n[numdiff] make install --> FAIL\n"
    exit 1
fi

printf "\n[numdiff] version %s installed in %s\n\n" "$NUMDIFF_VERSION" "$DEP_DIR"
exit 0
