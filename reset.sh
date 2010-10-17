#!/bin/bash

source "utils/libsubmodule.sh"

reset () {
  cd $1 && git reset --hard && git checkout master && cd - > /dev/null
}

git reset --hard
do_submodules reset
./init-submodules.sh
./pull.sh
