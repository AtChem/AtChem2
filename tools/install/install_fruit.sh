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

# This script downloads and installs FRUIT v3.4.3 into the directory
# given by input argument $1. This is dependent on the existence of a
# Ruby installation.
#
# N.B.: before running the script add the following lines to
# .bash_profile (or .profile), then restart the shell:
#   GEM_HOME=$HOME/.gem
#   PATH=$PATH:$GEM_HOME/bin
#
# Example usage:
#   ./install_fruit.sh /path/to/install/directory

if [ -z "$1" ] ; then
  echo "Please provide an argument to tools/install/install_fruit.sh"
  exit 1
fi
cd $1

wget https://iweb.dl.sourceforge.net/project/fortranxunit/fruit_3.4.3/fruit_3.4.3.zip
if [ $? -ne 0 ] ; then
  echo "wget of fruit failed"
  exit 1
fi

unzip -q fruit_3.4.3.zip
rm fruit_3.4.3.zip

cd fruit_3.4.3
gem install rake
cd fruit_processor_gem
rake install

exit 0
