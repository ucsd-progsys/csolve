#!/bin/bash

source "utils/libsubmodule.sh"

MESSAGE=$1

commit () {
    if [ "$MESSAGE" = "" ]
    then
        git commit -a
    else
        git commit -a -m "$MESSAGE"
    fi
}

commit_submodule () {
    cd $1
    commit
    cd - > /dev/null
}

do_submodules commit_submodule && commit