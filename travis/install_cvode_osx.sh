#!/bin/sh

# $1 is the directory to save cvode into
if [ -z "$1" ] ; then
  echo "Please provide an argument to travis/install_cvode_linux.sh"
  exit 1
fi
cd $1
wget https://computation.llnl.gov/projects/sundials/download/cvode-2.9.0.tar.gz

tar -zxf cvode-2.9.0.tar.gz
cd cvode-2.9.0/
mkdir build
cd build
cmake -DCMAKE_INSTALL_PREFIX=/Users/travis/build/AtChem/AtChem/cvode \
    -DCMAKE_C_COMPILER:FILEPATH=gcc \
    -DCMAKE_Fortran_COMPILER=/usr/local/Cellar/gcc@4.8/4.8.5/bin/gfortran-4.8 \
    -DLAPACK_ENABLE:BOOL=ON \
    -DFCMIX_ENABLE:BOOL=ON \
    -DEXAMPLES_ENABLE:BOOL=OFF \
    -DCMAKE_MACOSX_RPATH:BOOL=ON \
    ..
make -j8
make install
