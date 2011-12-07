#!/bin/sh

cd `dirname $0`/../../../external/gnu-coreutils/src/ || exit 1

if [ "$1" = "init" ]; then
    make clean && \
        cd .. && \
        ./lcc.configure && \
        exit 0
fi

NAME=`basename $0 ".sh"`

if [ "$NAME" = "makeCoreUtil" ]; then
    exit 0
fi

./make.lcc $NAME
