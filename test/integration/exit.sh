#!/bin/bash

root="$(dirname "${BASH_SOURCE[0]}")"/../..
source "$root"/test/integration/common.sh

_exit() {
    stack exec -- utils exit "$@"
}

_true() {
    stack exec -- utils true "$@"
}

_false() {
    stack exec -- utils false "$@"
}


ptest_exit() {
    _exit
    (( $? == 0 )) || die "Unexpected exit code"
}

ptest_exit_code_valid() {
    _exit 5
    (( $? == 5 )) || die "Unexpected exit code"
}

ptest_exit_code_high() {
    _exit 999
    (( $? == 255 )) || die "Unexpected exit code"
}

ptest_true() {
    _true || die "true returned failure"
}

ptest_false() {
    _false && die "false returned success"
    true
}


run_tests exit
