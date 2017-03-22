#!/bin/sh

# $1 is the input file
echo $1
echo "dir"

echo ./tools/run_atchem.py
echo "call run_atchem.py"
python ./tools/run_atchem.py $1
echo "cd"
echo "make"
make
