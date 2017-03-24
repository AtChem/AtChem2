#!/bin/sh

# $1 is the input file
cd $1
wget http://computation.llnl.gov/projects/sundials/download/cvode-2.9.0.tar.gz

tar -zxf cvode-2.9.0.tar.gz
cd cvode-2.9.0/
mkdir build
cd build
make clean
cmake -DCMAKE_INSTALL_PREFIX=/Users/travis/build/AtChem/AtChem/cvode \
    -DCMAKE_C_COMPILER:FILEPATH=gcc \
    -DCMAKE_Fortran_COMPILER=/usr/local/Cellar/gcc@4.8/4.8.5/bin/gfortran-4.8 \
    -DLAPACK_ENABLE:BOOL=ON \
    -DFCMIX_ENABLE:BOOL=ON \
    -DCMAKE_MACOSX_RPATH:BOOL=ON \
    ..
make -j8
make install
