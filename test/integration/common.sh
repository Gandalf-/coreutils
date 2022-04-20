#!/bin/bash

die() {
    echo "$*" >&2
    exit 1
}

expect() {
    grep -q "$1" ||
        die "stdin didn't contain $1"
}

expect-file() {
    [[ "$( cat "$1" )" == $2 ]] ||
        die "$1 didn't contain $2"
}

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
