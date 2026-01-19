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

# This script checks the format of the python scripts using the code formatter
# tool 'black': https://pypi.org/project/black/
#
# NB: the script must be run from the *Main Directory* of AtChem2.
# -----------------------------------------------------------------------------

python -c "import black"
if [ $? -ne 0 ] ; then
    printf "\n[ERROR] python formatter missing -- to install it, run the command:\n"
    printf "pip install black\n"
    exit 1
fi

printf "\nExecuting Python format test:\n"

test_total=0
test_fail=0

find build/ tools/ -name "*.py" | {
    while IFS= read -r file; do
        test_total=$((test_total + 1))
        black --check "$file" > /dev/null 2>&1
        if [ $? -eq 0 ] ; then
            printf "[PASS] %s\n" "$file"
        else
            printf "[FAIL] %s\n" "$file"
            test_fail=$((test_fail + 1))
        fi
    done

    printf "==> Python format test:\t"
    if [ "$test_fail" -eq 0 ]; then
        printf "PASSED [%s/%s scripts failed the test]\n" "$test_fail" "$test_total"
        test_script_pass=0
    else
        printf "FAILED [%s/%s scripts failed the test]\n" "$test_fail" "$test_total"
        test_script_pass=1
    fi

    exit $test_script_pass
}
