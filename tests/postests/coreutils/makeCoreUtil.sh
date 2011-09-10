cd `dirname $0`/../../../external/gnu-coreutils/src/ && \
    make clean && \
    cd .. && \
    ./lcc.configure && \
    cd src && \
    ./make.lcc $1
