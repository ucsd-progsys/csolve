#!/bin/bash

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
CMD="LPATH=\`mktemp -d -t\` && \
cd \$LPATH && \
tar zxf - && \
$WHAT"
tar czfh - -X utils/package-exclude . | ssh $WHERE $CMD
