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

# This script creates a new FruitProcessor object, and generates files
# to control the unit tests.
require 'rubygems'
require 'fruit_processor'

if ENV['TRAVIS'] == nil
  load "fruit_3.4.3/rake_base.rb"
else
  if ENV['TRAVIS_OS_NAME'] == 'linux'
    load "/home/travis/build/AtChem/AtChem2/fruit_3.4.3/rake_base.rb"
  else
    load "/Users/travis/build/AtChem/AtChem2/fruit_3.4.3/rake_base.rb"
  end
end

# fruit.f90 and fruit_util.f90 must exist in this directry.
$build_dir = ""  # if not set, build will be done in ../build/
# $source_dirs = ["../src", "./"]  # "../src" is for fruit.f90 and fruit_util.f90
$goal = "fruit_driver_dummy.exe"

# Add this line at the beginning if there are generated code involved
fp = FruitProcessor.new
#fp.shuffle = true          # if true order of methods will be random
fp.pre_process              # generates fruit_*_gen.f90

#eof
