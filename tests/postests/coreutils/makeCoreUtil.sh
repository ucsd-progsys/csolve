#!/bin/sh

NAME=`basename $0 ".sh"`

if [ "$NAME" = "makeCoreUtil" ]; then
    exit 0
fi

cd `dirname $0`/../../../external/gnu-coreutils/src/ && \
    make clean && \
    cd .. && \
    ./lcc.configure && \
    cd src && \
    ./make.lcc $NAME
