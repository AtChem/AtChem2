#!/bin/sh

# $1 is the input file
echo $1
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
echo "call run_atchem.py"
python $DIR/run_atchem.py $1
echo "cd"
cd $DIR/..
echo "make"
make
echo "run"
./atchem
echo "complete"
