#!/bin/sh

# This script downloads and installs NUMDIFF 5.8.1 into the directory given by input
# argument $1. This is dependent on the existence of a gcc installation.
#
# This is the macOSX version.
#
# Edit FORT_COMP to point to an existing gfortran or ifort executable.
#
# Example usage:
#   ./install_numdiff_linux.sh /path/to/install/directory
FORT_COMP=/usr/local/Cellar/gcc@4.8/4.8.5/bin/gfortran-4.8

cd $1
wget http://ftp.igh.cnrs.fr/pub/nongnu/numdiff/numdiff-5.8.1.tar.gz

tar -zxf numdiff-5.8.1.tar.gz
cd numdiff-5.8.1/
./configure --prefix=$1/numdiff
make
make install
