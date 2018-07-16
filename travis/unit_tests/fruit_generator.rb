require 'rubygems'
require 'fruit_processor'
if ENV['TRAVIS'] == nil
  load "~/libs/fruit_3.4.3/rake_base.rb"
else
  if ENV['TRAVIS_OS_NAME'] == 'linux'
    load "/home/travis/build/AtChem/AtChem2/fruit/fruit_3.4.3/rake_base.rb"
  else
    load "/Users/travis/build/AtChem/AtChem2/fruit/fruit_3.4.3/rake_base.rb"
  end
end
# fruit.f90 and fruit_util.f90 must exist in this directry.

$build_dir = ""  #If not set, build will be done in ../build/
# $source_dirs = ["../src", "./"]  # "../src" is for fruit.f90 and fruit_util.f90
$goal = "fruit_driver_dummy.exe"

# Add this line at the beginning if there are generated code involved
fp = FruitProcessor.new
#fp.shuffle = true          #if true order of methods will be random
fp.pre_process              #It generates fruit_*_gen.f90


#eof
