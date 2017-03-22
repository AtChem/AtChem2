#!/bin/sh

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $DIR/..
echo "run"
./atchem
echo "complete"
