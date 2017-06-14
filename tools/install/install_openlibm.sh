#!/bin/sh

# This script downloads and installs openlibm into the directory given by input
# argument $1. This is dependent on the existence of a gcc installation.
#
# This is the macOSX version.
#
# Edit FORT_COMP to point to an existing gfortran or ifort executable.
#
# Example usage:
#   ./install_openlibm.sh /path/to/install/directory

cd $1
wget https://github.com/JuliaLang/openlibm/archive/v0.4.1.tar.gz
tar -zxvf v0.4.1.tar.gz
rm v0.4.1.tar.gz
cd openlibm-0.4.1/
make -j
