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
# This script downloads and installs FRUIT into the Dependencies
# Directory, specified by input argument `$1`.
#
# Website: https://sourceforge.net/projects/fortranxunit/
# Requirements: wget, unzip, Ruby (v3.0 or earlier)
#
# BEFORE running the script, ensure that the RubyGems path is set by adding the
# following lines to `~/.bash_profile` (or `~/.profile`):
#   export GEM_HOME=$HOME/.gem
#   export PATH=$PATH:$GEM_HOME/bin
#
# Usage:
#   ./install_fruit.sh ~/path/to/dependencies/directory
# -----------------------------------------------------------------------------

FRUIT_VERSION="3.4.3"

# set path to the dependencies directory
if [ -z "$1" ] ; then
    printf "\n[fruit] missing argument: path to dependencies directory\n"
    exit 1
else # convert to absolute path and check that directory exists
    DEP_DIR=$(cd "$1" 2>/dev/null && pwd)
    if [ -z "$DEP_DIR" ] ; then
        printf "\n[fruit] %s does not exist\n" "$1"
        exit 1
    fi
    cd "$DEP_DIR" || exit 1
fi

# download archive
FRUIT_RELEASE="fruit_${FRUIT_VERSION}"
FRUIT_ARCHIVE="${FRUIT_RELEASE}.zip"
if ! wget "https://downloads.sourceforge.net/project/fortranxunit/${FRUIT_RELEASE}/${FRUIT_ARCHIVE}" ; then
    printf "\n[fruit] wget --> FAIL\n"
    exit 1
fi

# unpack archive
if ! unzip -q "$FRUIT_ARCHIVE" ; then
    printf "\n[fruit] unzip --> FAIL\n"
    exit 1
fi
rm -f "$FRUIT_ARCHIVE"

# compile and install
cd "${FRUIT_RELEASE}" || exit 1
if ! gem install rake ; then
    printf "\n[fruit] gem install rake --> FAIL\n"
    exit 1
fi

cd fruit_processor_gem || exit 1
if ! rake install ; then
    printf "\n[fruit] rake install --> FAIL\n"
    exit 1
fi

printf "\n[fruit] version %s installed in %s\n\n" "$FRUIT_VERSION" "$DEP_DIR"
exit 0
