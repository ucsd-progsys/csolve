#!/bin/bash

source "utils/libsubmodule.sh"

pull () {
    cd $1 && git pull && cd - > /dev/null
}

do_submodules pull && git pull && git submodule update