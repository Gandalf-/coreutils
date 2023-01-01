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
    [[ "$( cat "$1" )" == $2 ]] || {
        echo \'
        cat $1
        echo \' != \'
        cat <<< $2
        echo \'
        die $3
    }
}

equal() {
    [[ "$1" == "$2" ]] || die "$1 !=\n$2, $3"
}

_compare() {
    local mine="$(
        eval "timeout 1 stack exec utils -- $name ${@:2}" | md5sum
    )"

    local system="$(
        eval "timeout 1 $real ${@:2}" | md5sum
    )"

    [[ "$mine" == "$system" ]] || {
        echo "failure, $*"

        eval "timeout 1 stack exec utils -- $name ${@:2}"
        echo "==================================="
        eval "timeout 1 $real ${@:2}"

        exit 1
    }
}

compare() {
    _compare "${FUNCNAME[1]}" "$@"
}

skip() {
    :
}

run_tests() {
    local suite="$1"
    local executed=0
    local failures=0
    declare -A pids

    while read -r t; do
        case "$t" in
            ptest_*)
                (( executed++ ))
                ( $t >/dev/null 2>&1 ) &
                pids[$!]="$t"
            ;;
        esac
    done < <( declare -F | awk '{ print $3 }' )

    for pid in "${!pids[@]}"; do
        wait "$pid" || {
            (( failures++ ))

            local test=${pids[$pid]}
            _name="${test/ptest_/}"
            _name="${_name//_/ }"
            printf "\tfailure: $_name\n"
            ( $test )
        }
    done

    printf '%s\tpass: %3d fail: %3d\n' \
        "$suite" \
        $(( executed - failures )) \
        $failures
}

TEMPORARIES=()

temp() {
    while [[ $1 ]]; do
        local out="$(
            case $OSTYPE in
            darwin*)
                mktemp -t coreutils-${FUNCNAME[1]}
                ;;
            *)
                mktemp -p /tmp coreutils-${FUNCNAME[1]}-XXXXX
                ;;
            esac
        )"
        TEMPORARIES+=( "$out" )
        eval "$1"="$out"
        shift
    done
}

_cleanup() {
    rm -f "${TEMPORARIES[@]}"
}

trap _cleanup EXIT

cd "$root"
