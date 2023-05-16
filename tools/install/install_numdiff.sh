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
# This script downloads and installs numdiff into the directory given
# by input argument $1.
#
# Website: https://www.nongnu.org/numdiff/
# Version: 5.9.0
# Requirements: GCC, make
#
# Usage:
#   ./install_numdiff.sh /path/to/install/directory
# -----------------------------------------------------------------------------

# download numdiff archive to given directory (argument $1)
if [ -z "$1" ] ; then
  echo "Please provide an argument to ./install_numdiff.sh"
  exit 1
fi
cd $1
wget https://savannah.nongnu.org/download/numdiff/numdiff-5.9.0.tar.gz
if [ $? -ne 0 ] ; then
  echo "[numdiff] wget --- failed"
  exit 1
fi

# unpack numdiff archive
tar -zxf numdiff-5.9.0.tar.gz
if [ $? -ne 0 ] ; then
  echo "[numdiff] untar --- failed"
  exit 1
fi
rm numdiff-5.9.0.tar.gz

# compile and install numdiff
cd numdiff-5.9.0/
if [ "$(uname -s)" = 'Darwin' ]; then
  ./configure --prefix=$1/numdiff CPPFLAGS=-I/usr/local/Cellar/gettext/0.20.1/include/ LDFLAGS=-L/usr/local/Cellar/gettext/0.20.1/lib
else
  ./configure --prefix=$1/numdiff
fi
if [ $? -ne 0 ] ; then
  echo "[numdiff] configure --- failed"
  exit 1
fi

make
if [ $? -ne 0 ] ; then
  echo "[numdiff] make --- failed"
  exit 1
fi

make install
if [ $? -ne 0 ] ; then
  echo "[numdiff] make install --- failed"
  exit 1
fi

exit 0
