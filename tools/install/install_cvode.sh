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
# This script downloads and installs CVODE (part of SUNDIALS) into the
# directory given by input argument $1.
#
# The default Fortran compiler used by this script is gfortran. A
# different compiler can be specified by optional input argument $2.
#
# Website: https://computing.llnl.gov/projects/sundials/
# Version: 2.9.0 (SUNDIALS v2.7.0)
# Requirements: Fortran compiler (gfortran), cmake, make
#
# Usage:
#   ./install_cvode.sh /path/to/install/directory
#   ./install_cvode.sh /path/to/install/directory /path/to/fortran/compiler
# -----------------------------------------------------------------------------

# Ensure that the first argument (installation directory) is provided,
# and that it is an existing directory
if [ -z "$1" ] ; then
  echo "Please provide an argument to ./install_cvode.sh"
  echo "Example usage: ./install_cvode.sh /path/to/install/directory /path/to/fortran/compiler"
  exit 1
elif [ ! -d "$1" ]; then
  echo "$1 is not a directory - please create it if you want to install CVODE there."
  echo "Example usage: ./install_cvode.sh /path/to/install/directory /path/to/fortran/compiler"
  exit 1
fi
cvode_dir="$(cd "$(dirname "$1")"; pwd)/$(basename "$1")"

# Set the Fortran compiler. If the optional second argument is provided,
# check that it points to an existing executable. Otherwise, set the compiler
# to gfortran, and check that the executable exists.
if [ -z "$2" ] ; then
  if [ -f /usr/bin/gfortran ] ; then
    echo "Default fortran compiler (/usr/bin/gfortran) selected. To choose another compiler, provide it as a second argument."
    echo "Example usage: ./install_cvode.sh /path/to/install/directory /path/to/fortran/compiler"
  else
    echo "Default fortran compiler (/usr/bin/gfortran) selected, but it does not exist on this system."
    echo "Example usage: ./install_cvode.sh /path/to/install/directory /path/to/fortran/compiler"
    exit 1
  fi
  FORT_COMP=/usr/bin/gfortran
else
  FORT_COMP=$2
  if [ ! -f "$2" ] ; then
    echo $2 " selected as fortran compiler, but it is not a valid filename."
    echo "Example usage: ./install_cvode.sh /path/to/install/directory /path/to/fortran/compiler"
    exit 1
  fi
fi

# download SUNDIALS archive to given directory (argument $1)
cd $cvode_dir
wget -O sundials-2.7.0.tar.gz https://github.com/LLNL/sundials/archive/v2.7.0.tar.gz
if [ $? -ne 0 ] ; then
  echo "[sundials] wget --- failed"
  exit 1
fi

# unpack SUNDIALS archive
tar -zxf sundials-2.7.0.tar.gz
if [ $? -ne 0 ] ; then
  echo "[sundials] untar --- failed"
  exit 1
fi
rm sundials-2.7.0.tar.gz

# compile and install CVODE
cd sundials-2.7.0/
mkdir build/ && cd build/
cmake -DCMAKE_INSTALL_PREFIX=$cvode_dir/cvode \
      -DCMAKE_C_COMPILER:FILEPATH=gcc \
      -DCMAKE_Fortran_COMPILER=$FORT_COMP \
      -DBUILD_ARKODE:BOOL=OFF \
      -DBUILD_CVODE:BOOL=ON \
      -DBUILD_CVODES:BOOL=OFF \
      -DBUILD_IDA:BOOL=OFF \
      -DBUILD_IDAS:BOOL=OFF \
      -DBUILD_KINSOL:BOOL=OFF \
      -DLAPACK_ENABLE:BOOL=OFF \
      -DFCMIX_ENABLE:BOOL=ON \
      -DEXAMPLES_ENABLE:BOOL=OFF \
      -DCMAKE_MACOSX_RPATH:BOOL=ON \
      ..
if [ $? -ne 0 ] ; then
  echo "[cvode] cmake --- failed"
  exit 1
fi

make -j8
if [ $? -ne 0 ] ; then
  echo "[cvode] make --- failed"
  exit 1
fi

make install
if [ $? -ne 0 ] ; then
  echo "[cvode] make install --- failed"
  exit 1
fi

exit 0
