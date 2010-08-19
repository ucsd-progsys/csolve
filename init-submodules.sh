#!/bin/bash

source "utils/libsubmodule.sh"

checkout_master () {
    cd $1
    git checkout master
    cd - > /dev/null
}

git submodule update --init
do_submodules checkout_master