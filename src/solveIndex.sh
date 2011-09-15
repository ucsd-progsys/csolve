#!/bin/bash

DIR=`dirname $0`
LD_LIBRARY_PATH="$DIR/../external/z3/lib" "$DIR/indexSolver.native" $*