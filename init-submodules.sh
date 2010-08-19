#!/bin/bash

source "utils/libsubmodule.sh"

git submodule update --init --merge
do_submodules "cd $SUBMODULE; git checkout master; cd -"