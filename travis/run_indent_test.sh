#!/bin/bash

RESULTS_FILE=travis/tests/testsuite.log

echo "Running indent script on:"
for file in src/*.f90 ; do
  echo $file
  python3 ./tools/fix_indent.py $file $file.cmp &>/dev/null
  this_indent_file_failures=$(diff -q $file $file.cmp)
  exitcode=$?
  rm $file.cmp
  if [ $exitcode -eq 1 ]; then
    failed_indent="$failed_indent

$this_indent_file_failures"
    echo $file 'FAILED'
  elif [ $exitcode -ne 0 ]; then
    echo 'diff gave an error on' $file '. Aborting.'
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
