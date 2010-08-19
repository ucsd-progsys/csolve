#!/bin/bash

source "utils/libsubmodule.sh"

commit () {
    cd $1
    git commit -a
    cd - > /dev/null
}

do_submodules commit && git commit -a