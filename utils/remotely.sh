#!/bin/sh

if [ $# -le 1 ]
then
    echo "Usage: $0 [HOST] [COMMAND]"
    exit 1
fi

BASE=`dirname $0`
WHERE=$1
shift
WHAT=$*

cd $BASE/../
CMD="LPATH=`mktemp -t`;
mkdir \$LPATH;
cd \$LPATH;
tar zxf -;
export LD_LIBRARY_PATH=\$LPATH/external/z3/lib/;
$WHAT"
tar czfh - -X utils/package-exclude . | ssh $WHERE $CMD