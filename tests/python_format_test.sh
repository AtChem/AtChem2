#!/bin/sh
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

# This script checks the format of the python scripts using the tool `black`:
# https://pypi.org/project/black/
#
# NB: the script must be run from the *Main Directory* of AtChem2.
# -----------------------------------------------------------------------------

python -c "import black"
if [ $? -ne 0 ] ; then
    printf "\n[ERROR] python formatter tool missing"
    printf "r the command: pip install black\n"
    exit 1
fi

printf "\nChecking format of python scripts:"

test_total=0
test_fail=0

while IFS= read -r -d '' file; do
    ((test_total++))
    black --check "$file" &>/dev/null
    if [ $? -eq 0 ] ; then
        printf "\n[PASS] %s" "$file"
    else
        printf "\n[FAIL] %s" "$file"
        ((test_fail++))
    fi
done < <(find . -name "*.py" -print0)

if [ "$test_fail" -eq 0 ]; then
  printf "\n==> Python format test PASSED [%s/%s]\n" "$test_fail" "$test_total"
  test_pass=0
else
  printf "\n==> Python format test FAILED [%s/%s]\n" "$test_fail" "$test_total"
  test_pass=1
fi

exit $test_pass
