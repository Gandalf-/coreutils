#!/bin/bash

die() {
    echo "$*" >&2
    exit 1
}

expect() {
    grep -q "$1" ||
        die "stdin didn't contain $1"
}

expect-not() {
    grep -v -q "$1" ||
        die "stdin unexpectedly contained $1"
}

expect-empty() {
   out="$( sha1sum )"
   [[ "$out" =~ da39a3ee5e6b4b0d3255bfef95601890afd80709 ]] ||
        die "stdin wasn't empty"
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
