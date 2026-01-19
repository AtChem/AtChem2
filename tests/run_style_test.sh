#!/bin/bash
# -----------------------------------------------------------------------------
#
# Copyright (c) 2017-2025 Sam Cox, Roberto Sommariva
#
# This file is part of the AtChem2 software package.
#
# This file is licensed under the MIT license, which can be found in the file
# `LICENSE` at the top level of the AtChem2 distribution.
#
# -----------------------------------------------------------------------------

# This script executes the style test on the Fortran files to ensure that
# they conform to the AtChem2 coding guidelines, described in the manual
# (`doc/AtChem2-Manual.pdf`)
#
# NB: the script must be run from the *Main Directory* of AtChem2.
# -----------------------------------------------------------------------------

LOG_FILE=tests/styletest.log

echo "Executing style script on:" > $LOG_FILE
echo "" >> $LOG_FILE

for file in src/*.f90 ; do
  echo $file >> $LOG_FILE
  python3 ./tools/fix_style.py $file $file.cmp &>/dev/null
  this_style_file_failures=$(diff -q $file $file.cmp)
  exitcode=$?
  rm $file.cmp
  if [ $exitcode -eq 1 ]; then
    failed_style="$failed_style

$this_style_file_failures"
    echo $file "FAILED" >> $LOG_FILE
  elif [ $exitcode -ne 0 ]; then
    echo "diff gave an error on" $file ". Aborting." >> $LOG_FILE
    exit 1
  fi
done

if [ -z "$failed_style" ]; then
  echo "==> Style test PASSED"
  style_test_passed=0
else
  echo "==> Style test FAILED"
  echo "$failed_style" >> $LOG_FILE
  style_test_passed=1
fi
echo "" >> $LOG_FILE
echo "Execution of style script finished." >> $LOG_FILE

echo "==> Style test logfile:" $LOG_FILE
exit $style_test_passed
