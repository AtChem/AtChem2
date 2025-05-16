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
# This script downloads and installs FRUIT into the directory given by
# input argument $1.
#
# Website: https://sourceforge.net/projects/fortranxunit/
# Version: 3.4.3
# Requirements: Ruby
#
# N.B.: before running the script, ensure that the RubyGems paths are
# set in ~/.bash_profile (or ~/.profile):
#   GEM_HOME=$HOME/.gem
#   PATH=$PATH:$GEM_HOME/bin
#
# Usage:
#   ./install_fruit.sh /path/to/install/directory
# -----------------------------------------------------------------------------

# download FRUIT archive to given directory (argument $1)
if [ -z "$1" ] ; then
  echo "Please provide an argument to ./install_fruit.sh"
  exit 1
fi
cd $1
wget https://downloads.sourceforge.net/project/fortranxunit/fruit_3.4.3/fruit_3.4.3.zip
if [ $? -ne 0 ] ; then
  echo "[fruit] wget --- failed"
  exit 1
fi

# unpack FRUIT archive
unzip -q fruit_3.4.3.zip
if [ $? -ne 0 ] ; then
  echo "[fruit] unzip --- failed"
  exit 1
fi
rm fruit_3.4.3.zip

# compile and install FRUIT
cd fruit_3.4.3/
gem install rake
if [ $? -ne 0 ] ; then
  echo "[fruit] gem install rake --- failed"
  exit 1
fi

cd fruit_processor_gem/
rake install
if [ $? -ne 0 ] ; then
  echo "[fruit] rake install --- failed"
  exit 1
fi

exit 0
