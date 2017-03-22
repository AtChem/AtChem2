#!/bin/sh

# $1 is the input file
echo $1
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
echo "dir"
echo $DIR

echo $DIR/tools/run_atchem.py
echo "call run_atchem.py"
python $DIR/tools/run_atchem.py $1
echo "cd"
cd $DIR/..
echo "make"
make
