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

#!/bin/sh

# This script downloads and installs OPENLIBM 0.4.1 into the directory
# given by input argument $1. This is dependent on the existence of a
# gcc installation.
#
# Example usage:
#   ./install_openlibm.sh /path/to/install/directory

if [ -z "$1" ] ; then
  echo "Please provide an argument to tools/install/install_openlibm.sh"
  exit 1
fi
cd $1
wget https://github.com/JuliaLang/openlibm/archive/v0.4.1.tar.gz
if [ $? -ne 0 ] ; then
  echo "wget of openlibm failed"
  exit 1
fi

tar -zxf v0.4.1.tar.gz
rm v0.4.1.tar.gz

cd openlibm-0.4.1/
make -j

exit 0
