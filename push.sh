#!/bin/bash

source "utils/libsubmodule.sh"

push () {
    cd $1 && git push -q && cd - > /dev/null
}

do_submodules push && git push