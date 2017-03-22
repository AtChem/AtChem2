#!/bin/sh

# $1 is the input file
set -e
echo $1

echo "call ./tools/setup_atchem.py"
python ./tools/setup_atchem.py $1
echo "make"
make
