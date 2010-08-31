#!/bin/bash

source "utils/libsubmodule.sh"

push () {
    cd $1 && git push && cd - > /dev/null
}

do_submodules push && git push
