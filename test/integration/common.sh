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
        die "$1 didn't contain $2, $3"
}

equal() {
    [[ "$1" == "$2" ]] || die "$1 !=\n$2, $3"
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

compare() {
    test "${FUNCNAME[1]}" "$@"
}

skip() {
    :
}

run_tests() {
    local suite="$1"
    local executed=0
    local failures=0

    while read -r t; do
        case "$t" in
            test_*)
                (( executed++ ))
                ( $t >/dev/null 2>&1 ) || {
                    (( failures++ ))
                    local name="${t/test_/}"
                    echo "\tfailure: ${name//_/ }"
                }
            ;;
        esac
    done < <( declare -F | awk '{ print $3 }' )

    echo "$suite tests complete pass: $(( executed - failures )) fail: $failures"
}

cd "$root"
