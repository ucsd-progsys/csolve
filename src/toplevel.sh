#!/bin/bash

DIR=`dirname $0`
cd $DIR/_build
LD_LIBRARY_PATH="../../external/z3/lib" "../csolve.top" $*
