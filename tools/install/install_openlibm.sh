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
# This script downloads and installs openlibm into the directory
# given by input argument $1.
#
# Website: https://openlibm.org/
# Version: 0.8.1
# Requirements: GCC, make
#
# Usage:
#   ./install_openlibm.sh /path/to/install/directory
# -----------------------------------------------------------------------------

# download openlibm archive to given directory (argument $1)
if [ -z "$1" ] ; then
  echo "Please provide an argument to ./install_openlibm.sh"
  exit 1
fi
cd $1
wget https://github.com/JuliaMath/openlibm/archive/v0.8.1.tar.gz
if [ $? -ne 0 ] ; then
  echo "wget openlibm --- failed"
  exit 1
fi

# unpack openlibm archive
tar -zxf v0.8.1.tar.gz
rm v0.8.1.tar.gz

# compile openlibm
cd openlibm-0.8.1/
make -j

exit 0
