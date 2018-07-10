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

# This script downloads and installs FRUIT 3.4.3 into the directory
# given by input argument $1.
#
# Example usage:
#   ./install_fruit.sh /path/to/install/directory /path/to/rvm/directory

rvm install ruby-2.4.2
wget https://kent.dl.sourceforge.net/project/fortranxunit/fruit_3.4.3/fruit_3.4.3.zip
mkdir -p $1
unzip -q fruit_3.4.3.zip -d $1
cd $1/fruit_3.4.3/fruit_processor_gem/
rake install
rake
