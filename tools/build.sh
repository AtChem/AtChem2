#!/bin/sh
#
# $1 is the input file
# $2 is the directory in which to place the mechanism-rate-coefficients.f90 and
#    mechanism-rate-declarations.f90 files. This, by default, points
#    to the base directory of the source files of AtChem.
# $3 is ...

set -e
echo ''
echo $1
echo $2
echo $3

if [ "${PWD##*/}" == "tools" ]; then
    echo "call ./setup_atchem.py"
    python ./setup_atchem.py $1 $2 $3
else
    echo "call ./tools/setup_atchem.py"
    python ./tools/setup_atchem.py $1 $2 $3
fi

echo ''
echo "make" $1
make --always-make
