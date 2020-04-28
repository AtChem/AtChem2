#!/bin/bash
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

# This script ...

RESULTS_FILE=travis/tests/testsuite.log

echo "Running style script on:"
for file in src/*.f90 ; do
  echo $file
  python ./build/fix_style.py $file $file.cmp &>/dev/null
  this_style_file_failures=$(diff -q $file $file.cmp)
  exitcode=$?
  rm $file.cmp
  if [ $exitcode -eq 1 ]; then
    failed_style="$failed_style

$this_style_file_failures"
    echo $file "FAILED"
  elif [ $exitcode -ne 0 ]; then
    echo "diff gave an error on" $file ". Aborting."
    exit 1
  fi
done

echo ""
if [ -z "$failed_style" ]; then
  echo "Style test PASSED"
  style_test_passed=0
else
  echo "Style test FAILED"
  echo "$failed_style" >> $RESULTS_FILE
  style_test_passed=1
fi
echo "Style script finished"
echo ""
exit $style_test_passed
