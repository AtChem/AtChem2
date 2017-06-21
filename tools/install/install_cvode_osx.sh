#!/bin/sh

# This script downloads and installs CVODE 2.9.0 into the directory
# given by input argument $1. Using the fortran compiler executable
# given by input argument $2.
#
# This is the macOSX version.
#
# Example usage:
#   ./install_cvode_osx.sh /path/to/install/directory /path/to/fortran/compiler

OS=$(uname -s)
echo $OS
if [ "$OS" == 'Darwin' ]; then
  LAPACK_LIBS=/usr/lib/liblapack.dylib:/usr/lib/libblas.dylib
else
  LAPACK_LIBS=/usr/lib/liblapack.so:/usr/lib/libblas.so
fi
echo $LAPACK_LIBS
if [ -z "$1" ] ; then
  echo "Please provide an argument to tools/install/install_cvode_osx.sh"
  echo "Example usage: ./install_cvode_osx.sh /path/to/install/directory /path/to/fortran/compiler"
  exit 1
elif [ ! -d "$1" ]; then
  echo "$1 is not a directory"
  echo "Example usage: ./install_cvode_osx.sh /path/to/install/directory /path/to/fortran/compiler"
  exit 1
fi
if [ -z "$2" ] ; then
  if [ -f /usr/bin/gfortran ] ; then
    echo "Default fortran compiler executable selected. To choose another compiler, provide this as a second argument."
    echo "Example usage: ./install_cvode_osx.sh /path/to/install/directory /path/to/fortran/compiler"
  else
    echo "Default fortran compiler 'gfortran' executable selected, but this is not a valid filename."
    echo "Example usage: ./install_cvode_osx.sh /path/to/install/directory /path/to/fortran/compiler"
    exit 1
  fi
  FORT_COMP=/usr/bin/gfortran
else
  FORT_COMP=$2
  if [ ! -f "$2" ] ; then
    echo $2 " selected as fortran compiler, but this is not a valid filename."
    echo "Example usage: ./install_cvode_osx.sh /path/to/install/directory /path/to/fortran/compiler"
    exit 1
  fi
fi
exit 1
cd $1
ls -al /usr/bin
wget https://computation.llnl.gov/projects/sundials/download/cvode-2.9.0.tar.gz

tar -zxf cvode-2.9.0.tar.gz
rm cvode-2.9.0.tar.gz

cd cvode-2.9.0/
mkdir build

cd build
cmake -DCMAKE_INSTALL_PREFIX=$1/cvode \
    -DCMAKE_C_COMPILER:FILEPATH=gcc \
    -DCMAKE_Fortran_COMPILER=$FORT_COMP \
    -DLAPACK_ENABLE:BOOL=ON \
    -DLAPACK_LIBRARIES=$LAPACK_LIBS \
    -DFCMIX_ENABLE:BOOL=ON \
    -DEXAMPLES_ENABLE:BOOL=OFF \
    -DCMAKE_MACOSX_RPATH:BOOL=ON \
    ..
make -j8
make install
