#!/bin/bash
#set -e
function test_output_text {
  # This file creates 2 temporary files, which hold the section of
  # each of the input files (arg 1 and 2) defined by the args 3 and 4 for beginning
  # and end line numbers. It then numdiffs these using test_output_file
  # $1 first file for comparison
  # $2 second file for comparison
  # $3 start line number of section to compare
  # $4 end line number of section to compare
  # $5 name of test
  file1=travis/tests/$5/temporary_file.tmp
  file2=travis/tests/$5/temporary_file.tmp.cmp
  ndselect -b $3 -e $4 -o $file1 $1
  ndselect -b $3 -e $4 -o $file2 $2
  # Save output of test_output_file
  temp_internal=$(test_output_file $file1 $file2)
  exitcode=$?
  echo "$temp_internal"
  # Clean up
  rm $file1 $file2
  return $exitcode
}

function test_output_file {
  # numdiff with relative tolerance given by -r argument
  numdiff -r 1.0e-10 $1 $2
}

function find_string {
  # This function finds the line number of the first occurence of the string
  # $1 in file $2
  grep -n $1 $2 | grep -Eo '^[^:]+'
}

# The basic workflow of this function is to loop over each test in $1, and for
# each test, compare the screen output and the other output files with previously held results.
# A mismatch generates a test failure. Numdiff is used to cope with small numerical
# differences due to differing hardware, OS, and package versions.
#
# $this_test_failures is used to keep a track of whether each test is passing: empty indicates
# a pass, while non-empty indicates a failure.
# $this_file_failures holds the output of the numdiff for each file
#
# We also allow some lines of the screen output to be skipped - e.g. the runtime line,
# since this is machine-dependent. Multiple such lines can be skipped by appending to
# $skip_test. This requires extra machinery to handle splitting the file into
# sections between the skipped lines, and to numdiff those sections.

echo "travis/test_runner.sh: Tests to be run:" $1
TESTS_DIR=travis/tests
RESULTS_FILE=$TESTS_DIR/results
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
  echo "set up and make" $TESTS_DIR/$test
  ./tools/build.sh $TESTS_DIR/$test.fac &> /dev/null

  # Run atchem with the argument pointing to the output directory
  echo Running   $TESTS_DIR/$test ...
  ./atchem $TESTS_DIR/$test > $TESTS_DIR/$test.out

  # Now begin the process of diffing the screen output file
  echo Comparing $TESTS_DIR/$test ...
  # This lists all words which will have their line skipped in the main output file. This is a space-delimited list.
  # TODO: extend to multi-word exclusions
  skip_text="Runtime ratesOutputStepSize"
  # Generate a table of line numbers to omit based on skip_text
  for item in $skip_text; do
    skip_line_number=$(find_string $item $TESTS_DIR/$test.out)
    list_of_skip_line_numbers="$list_of_skip_line_numbers $skip_line_number"
  done
  # Add one past the last line number
  list_of_skip_line_numbers="$list_of_skip_line_numbers $(($(grep -c '' $TESTS_DIR/$test.out)+1))"
  # Sort the list in numerical order
  sorted_list_of_skip_line_numbers=$(echo $list_of_skip_line_numbers | tr " " "\n" | sort -n)

  # Loop over the list of line numbers. Numdiff the section between the last
  # skipped line and the next skipped line. Save to $this_file_failures, and append to $this_test_failures
  # if there is a difference.
  # $old_skip_line_number keeps track of the previously skipped line
  old_skip_line_number=0
  for skip_line_number in $sorted_list_of_skip_line_numbers; do
    this_file_failures=$(test_output_text $TESTS_DIR/$test.out $TESTS_DIR/$test.out.cmp $(($old_skip_line_number+1)) $(($skip_line_number-1)) $test)
    exitcode=$?
    if [ $exitcode -eq 1 ]; then
      this_test_failures="$this_test_failures

Differences found in $TESTS_DIR/$test.out. Add $old_skip_line_number to the line numbers shown:
$this_file_failures"
    elif [ $exitcode -eq -1 ]; then
      echo 'Numdiff gave an error. Aborting.'
      exit 1
    fi
    old_skip_line_number=$skip_line_number
  done

  # loop over all files in output directory, and numdiff these. If the numdiff gives differences,
  # then add the numdiff output to $this_test_failures via $this_file_failures,.
  for filename in $TESTS_DIR/$test/*.output; do
    this_file_failures=$(test_output_file $filename $filename.cmp)
    exitcode=$?
    if [ $exitcode -eq 1 ]; then
      this_test_failures="$this_test_failures

$this_file_failures"
    elif [ $exitcode -eq -1 ]; then
      echo 'Numdiff gave an error. Aborting.'
      exit 1
    fi
  done

  # Pass if $this_test_failures is empty. Otherwise, append all of $this_test_failures to $RESULTS_FILE.
  # Increment the counters as necessary.
  if [ -z "$this_test_failures" ]; then
    echo "$test PASSED"
    pass_counter=$((pass_counter+1))
  else
    echo "$test FAILED"
    echo "Test name: $test" >> $RESULTS_FILE
    echo "$this_test_failures" >> $RESULTS_FILE
    fail_counter=$((fail_counter+1))
  fi

  echo "$this_test_failures"
done

# After all tests are run, exit with a FAIL if $fail_counter>0, otherwise PASS.
if [[ "$fail_counter" -gt 0 ]]; then
  echo "Tests FAILED"
  echo "$fail_counter/$test_counter tests FAILED"
  echo "The diff is as follows:"
  cat $RESULTS_FILE
  exit 1
else
  echo "Tests PASSED"
  exit 0
fi ;
