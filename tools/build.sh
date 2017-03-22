#!/bin/sh

# $1 is the input file
set -e
echo $1

echo "call ./tools/run_atchem.py"
python ./tools/run_atchem.py $1
echo "make"
make