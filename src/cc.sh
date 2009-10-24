#!/bin/sh
# Substitute for the cc command used in most Makefiles' rules for .c.o.
# DO NOT use to merge several .o files!
# Instead, use cilly --merge --keepmerged --noPrintLn

TMP=`tempfile`.c

cpp -nostdinc -include `dirname $0`/lib.h $1 > $TMP
cilly --merge --keepmerged --noPrintLn -E -P -c -o `basename $1 .c`.o $TMP
rm $TMP