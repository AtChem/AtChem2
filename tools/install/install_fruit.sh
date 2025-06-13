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
# This script downloads and installs FRUIT into the Dependencies
# Directory, specified by input argument `$1`.
#
# Website: https://sourceforge.net/projects/fortranxunit/
# Requirements: ruby
#
# NB: before running the script, ensure that the RubyGems paths are
#     set in ~/.bash_profile (or ~/.profile):
#       GEM_HOME=$HOME/.gem
#       PATH=$PATH:$GEM_HOME/bin
#
# Usage:
#   ./install_fruit.sh ~/path/to/dependencies/directory
# -----------------------------------------------------------------------------

FRUIT_VERSION="3.4.3"

# path to dependencies directory
if [ -z "$1" ] ; then
    printf "\n[fruit] missing argument: path to dependencies directory\n"
    exit 1
else
    DEP_DIR="$1"
    if ! cd "$DEP_DIR"; then
        printf "\n[fruit] $DEP_DIR does not exist\n"
        exit 1
    fi
fi

# download archive
FRUIT_DIR="fruit_${FRUIT_VERSION}"
FRUIT_ARCHIVE="${FRUIT_DIR}.zip"
wget "https://downloads.sourceforge.net/project/fortranxunit/${FRUIT_DIR}/${FRUIT_ARCHIVE}"
if [ $? -ne 0 ] ; then
    printf "\n[fruit] wget --> FAIL\n"
    exit 1
fi

# unpack archive
unzip -q "$FRUIT_ARCHIVE"
if [ $? -ne 0 ] ; then
    printf "\n[fruit] unzip --> FAIL\n"
    exit 1
fi
rm -f "$FRUIT_ARCHIVE"

# compile and install
cd "${FRUIT_DIR}"
gem install rake
if [ $? -ne 0 ] ; then
    printf "\n[fruit] gem install rake --> FAIL\n"
    exit 1
fi

cd fruit_processor_gem/
rake install
if [ $? -ne 0 ] ; then
    printf "\n[fruit] rake install --> FAIL\n"
    exit 1
fi

# finish installation
printf "\n[fruit] version %s installed in %s\n" "$FRUIT_VERSION" "$DEP_DIR"
exit 0
