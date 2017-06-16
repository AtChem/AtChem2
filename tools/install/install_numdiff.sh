#!/bin/sh

# This script downloads and installs NUMDIFF 5.8.1 into the directory
# given by input argument $1. This is dependent on the existence of a
# gcc installation.
#
# Example usage:
#   ./install_numdiff.sh /path/to/install/directory

if [ -z "$1" ] ; then
  echo "Please provide an argument to tools/install/install_numdiff.sh"
  exit 1
fi
cd $1
wget http://ftp.igh.cnrs.fr/pub/nongnu/numdiff/numdiff-5.8.1.tar.gz

tar -zxf numdiff-5.8.1.tar.gz
rm numdiff-5.8.1.tar.gz

cd numdiff-5.8.1/
./configure --prefix=$1/numdiff
make
make install
