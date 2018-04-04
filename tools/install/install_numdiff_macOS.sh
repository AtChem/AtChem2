# -----------------------------------------------------------------------------
#
# Copyright (c) 2018 Sam Cox, Roberto Sommariva
#
# This file is part of the AtChem2 software package.
#
# This file is covered by the MIT license which can be found in the file
# LICENSE.md at the top level of the AtChem2 distribution.
#
# -----------------------------------------------------------------------------

#!/bin/sh

# This script downloads and installs NUMDIFF 5.8.1 into the directory
# given by input argument $1. This is dependent on the existence of a
# gcc installation.
#
# Example usage:
#   ./install_numdiff_macOS.sh /path/to/install/directory

if [ -z "$1" ] ; then
  echo "Please provide an argument to tools/install/install_numdiff_macOS.sh"
  exit 1
fi
cd $1
wget http://ftp.igh.cnrs.fr/pub/nongnu/numdiff/numdiff-5.8.1.tar.gz

tar -zxf numdiff-5.8.1.tar.gz
rm numdiff-5.8.1.tar.gz

cd numdiff-5.8.1/
./configure --prefix=$1/numdiff CPPFLAGS=-I/usr/local/Cellar/gettext/0.19.8.1/include/ LDFLAGS=-L/usr/local/Cellar/gettext/0.19.8.1/libmake
make
make install
