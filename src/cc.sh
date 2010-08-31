#!/bin/sh
# Drop-in replacement for gcc that uses the cilly driver to merge
# files and run the Liquid C binary.

BASE=`dirname $0`

$BASE/../external/cil/bin/cilly --merge --doliquidc $@
