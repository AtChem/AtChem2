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

function test_output_text {
  # This function creates 2 temporary files, which hold the section of
  # each of the input files (arg 1 and 2) defined by the args 3 and 4
  # for beginning and end line numbers. It then numdiffs these using
  # test_output_file
  #
  # $1 first file for comparison
  # $2 second file for comparison
  # $3 start line number of section to compare
  # $4 end line number of section to compare
  # $5 name of test
  file1=tests/model_tests/$5/output/temporary_file.tmp
  file2=tests/model_tests/$5/output/temporary_file.tmp.cmp
  ndselect -b $3 -e $4 -o $file1 $1
  ndselect -b $3 -e $4 -o $file2 $2
  # Save output of test_output_file
  temp_internal=$(test_output_file $file1 $file2)
  exitcode=$?
  echo $temp_internal
  # Clean up
  rm $file1 $file2
  return $exitcode
}

function test_output_file {
  # This function executes numdiff with relative tolerance given by
  # the -r argument
  numdiff -a 1.e-12 -r 5.0e-05 $1 $2
}

function find_string {
  # This function finds the line number of the first occurence of the
  # string $1 in file $2
  grep -n $1 $2 | grep -Eo "^[^:]+"
}

# The basic workflow of this function is to loop over each test in $1, and for
# each test, compare the screen output and the other output files with previously held results.
# A mismatch generates a test failure. Numdiff is used to cope with small numerical
# differences due to differing hardware, OS, and package versions.
#
# $2 is used to pass CVODELIB in from Makefile, in order to be able to set DYLD_LIBRARY_PATH on macOS.
#
# $this_test_failures is used to keep a track of whether each test is passing: empty indicates
# a pass, while non-empty indicates a failure.
# $this_file_failures holds the output of the numdiff for each file
#
# We also allow some lines of the screen output to be skipped - e.g. the runtime line,
# since this is machine-dependent. Multiple such lines can be skipped by appending to
# $skip_test. This requires extra machinery to handle splitting the file into
# sections between the skipped lines, and to numdiff those sections.

export DYLD_LIBRARY_PATH=$2

TESTS_DIR=tests/model_tests
LOG_FILE=tests/modeltests.log

echo "Executing model tests script." > $LOG_FILE
echo "Model tests to run:" $1 >> $LOG_FILE

# initialise counters
test_counter=0
fail_counter=0
pass_counter=0

# loop over each test
for test in $1; do
  # reinitialise variables
  this_test_failures=""
  list_of_skip_line_numbers=""
  sorted_list_of_skip_line_numbers=""
  # increment test_counter
  test_counter=$((test_counter+1))
  echo "" >> $LOG_FILE
  echo "Set up and make" $TESTS_DIR/$test >> $LOG_FILE
  ./build/build_atchem2.sh $TESTS_DIR/$test/$test.fac $TESTS_DIR/$test/configuration/ mcm/ &> /dev/null
  exitcode=$?
  if [ $exitcode -ne 0 ]; then
    echo "Building" $test "test failed with exit code" $exitcode >> $LOG_FILE
    exit $exitcode
  fi
  # Run atchem2 with the argument pointing to the output directory
  echo "Running" $TESTS_DIR/$test "..." >> $LOG_FILE
  ./atchem2 --shared_lib=$TESTS_DIR/$test/configuration/mechanism.so --output=$TESTS_DIR/$test/output --configuration=$TESTS_DIR/$test/configuration --mcm=mcm --constraints=$TESTS_DIR/$test/constraints > $TESTS_DIR/$test/$test.out

  # Now begin the process of diffing the screen output file
  echo "Comparing" $TESTS_DIR/$test "..." >> $LOG_FILE
  # This lists all words which will have their line skipped in the main output file. This is a space-delimited list.
  # TODO: extend to multi-word exclusions
  skip_text="Runtime"
  # Generate a table of line numbers to omit based on skip_text
  for item in $skip_text; do
    skip_line_number=$(find_string $item $TESTS_DIR/$test/$test.out)
    list_of_skip_line_numbers="$list_of_skip_line_numbers $skip_line_number"
  done
  # Add one past the last line number
  list_of_skip_line_numbers="$list_of_skip_line_numbers $(($(grep -c "" $TESTS_DIR/$test/$test.out)+1))"
  # Sort the list in numerical order
  sorted_list_of_skip_line_numbers=$(echo $list_of_skip_line_numbers | tr " " "\n" | sort -n)

  # Loop over the list of line numbers. Numdiff the section between the last
  # skipped line and the next skipped line. Save to $this_file_failures, and append to $this_test_failures
  # if there is a difference.
  # $old_skip_line_number keeps track of the previously skipped line
  old_skip_line_number=0
  for skip_line_number in $sorted_list_of_skip_line_numbers; do
    this_file_failures=$(test_output_text $TESTS_DIR/$test/$test.out $TESTS_DIR/$test/$test.out.cmp $(($old_skip_line_number+1)) $(($skip_line_number-1)) $test)
    exitcode=$?
    echo "Checking" $TESTS_DIR/$test/output/$test.out "between lines" $(($old_skip_line_number+1)) "and" $(($skip_line_number-1)) >> $LOG_FILE
    if [ $exitcode -eq 1 ]; then
      this_test_failures="$this_test_failures

Differences found in $TESTS_DIR/$test/$test.out. Add $old_skip_line_number to the line numbers shown:
$this_file_failures"
    elif [ $exitcode -eq -1 ]; then
      echo "Numdiff gave an error. Aborting." >> $LOG_FILE
      exit 1
    fi
    old_skip_line_number=$skip_line_number
  done

  # Loop over all files in output directory, and numdiff these. If the numdiff gives differences,
  # then add the numdiff output to $this_test_failures via $this_file_failures,.
  for filename in $TESTS_DIR/$test/output/*.output.cmp; do
    echo "Checking" $filename >> $LOG_FILE
    this_file_failures=$(test_output_file ${filename: 0: ${#filename}-4} $filename)
    exitcode=$?
    if [ $exitcode -eq 0 ]; then
      continue
    elif [ $exitcode -eq 1 ]; then
      this_test_failures="$this_test_failures

$this_file_failures"
    else
      echo "Numdiff gave an error. Aborting." >> $LOG_FILE
      exit 1
    fi
  done

  # Loop over all files (that don't end in .cmp) in the reactionRates
  # subdirectory of output directory, and numdiff these. If the numdiff gives differences,
  # then add the numdiff output to $this_test_failures via $this_file_failures,.
  for filename in $TESTS_DIR/$test/output/reactionRates/* ; do
    if [ ${filename: -4} == ".cmp" ] ; then
      echo "Checking" $filename >> $LOG_FILE
      this_file_failures=$(test_output_file ${filename: 0: ${#filename}-4} $filename)
      exitcode=$?
      if [ $exitcode -eq 0 ]; then
        continue
      elif [ $exitcode -eq 1 ]; then
        this_test_failures="$this_test_failures

$this_file_failures"
      else
        echo "Numdiff gave an error. Aborting." >> $LOG_FILE
        exit 1
      fi
    fi
  done

  # Loop over all files (that fit mechanism.*.cmp) in the model/configuration
  # subdirectory of output directory, and numdiff these. If the numdiff gives differences,
  # then add the numdiff output to $this_test_failures via $this_file_failures,.
  for filename in $TESTS_DIR/$test/model/configuration/mechanism.* ; do
    # guard against empty filelist
    #[ -e "$filename" ] || continue
    if [ ${filename: -4} == ".cmp" ] ; then
      echo "Checking" $filename >> $LOG_FILE
      this_file_failures=$(test_output_file ${filename: 0: ${#filename}-4} $filename)
      exitcode=$?
      if [ $exitcode -eq 0 ]; then
        continue
      elif [ $exitcode -eq 1 ]; then
        this_test_failures="$this_test_failures

$this_file_failures"
      else
        echo "Numdiff gave an error. Aborting." >> $LOG_FILE
        exit 1
      fi
    fi
  done

  # Pass if $this_test_failures is empty. Otherwise, append all of $this_test_failures
  # to logfile (modeltests.log)). Increment the counters as necessary.
  if [ -z "$this_test_failures" ]; then
    echo $test "PASSED" >> $LOG_FILE
    echo "*" $test
    pass_counter=$((pass_counter+1))
  else
    echo $test "FAILED" >> $LOG_FILE
    echo "*" $test
    fail_counter=$((fail_counter+1))
  fi

  echo $this_test_failures >> $LOG_FILE
done

if [[ "$RUNNER_OS" == "Linux" ]]; then bash <(curl -s https://codecov.io/bash) -F tests ; fi

# After all tests are run, exit with a FAIL if $fail_counter>0, otherwise PASS.
if [[ "$fail_counter" -gt 0 ]]; then
  echo "==> Model tests FAILED [" $fail_counter/$test_counter "]"
  model_tests_passed=1
else
  echo "==> Model tests PASSED [" $test_counter/$test_counter "]"
  model_tests_passed=0
fi
echo "" >> $LOG_FILE
echo "Execution of model tests script finished." >> $LOG_FILE

exit $model_tests_passed
