#!/bin/sh

echo "CACM seems to be diverging in Z3 - TBD"
exit 1

cd ../tests/all/cacm/ && \
make clean && \
make
