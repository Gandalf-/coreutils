#!/bin/bash

test() {

    local mine="$(
        eval "timeout 1 stack exec utils -- $name ${@:2}" | md5sum
    )"

    local system="$(
        eval "timeout 1 $real ${@:2}" | md5sum
    )"

    [[ "$mine" == "$system" ]] || {
        echo "failure, $*"

        eval "timeout 1 stack exec utils -- $name ${@:2}" \
            | head -c 1000

        echo "==================================="

        eval "timeout 1 $real ${@:2}" \
            | head -c 1000
    }
}

skip() {
    :
}

cd "$root"
