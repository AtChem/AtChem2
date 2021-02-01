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

# This script executes the indent test on the Fortran files to ensure
# that they conform to the coding guidelines, described in the manual
# (doc/AtChem2-Manual.pdf)

RESULTS_FILE=test/tests/testsuite.log

echo "Running indent script on:"
for file in src/*.f90 ; do
  echo $file
  python ./tools/fix_indent.py $file $file.cmp &>/dev/null
  this_indent_file_failures=$(diff -q $file $file.cmp)
  exitcode=$?
  rm $file.cmp
  if [ $exitcode -eq 1 ]; then
    failed_indent="$failed_indent

$this_indent_file_failures"
    echo $file "FAILED"
  elif [ $exitcode -ne 0 ]; then
    echo "diff gave an error on" $file ". Aborting."
    exit 1
  fi
done

echo ""
if [ -z "$failed_indent" ]; then
  echo "Indent test PASSED"
  indent_test_passed=0
else
  echo "Indent test FAILED"
  echo "$failed_indent" >> $RESULTS_FILE
  indent_test_passed=1
fi
echo "Indent script finished"
echo ""
exit $indent_test_passed
