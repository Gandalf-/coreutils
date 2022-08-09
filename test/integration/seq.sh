#!/bin/bash

root="$(dirname "${BASH_SOURCE[0]}")"/../..
name=seq
real=/usr/bin/seq
source "$root"/test/integration/common.sh

ptest_simple() {
    compare '99'
    compare '45 99'
    compare '45 5 99'
}

ptest_decimal() {
    compare '1   0.25 5'
    compare '1.5 1    5'
    compare '1   1.9  5'
    compare '1   .9   5'
    compare '1   1    5.9'
}

ptest_reverse() {
    compare '5 -1 1'
    compare '5 -0.5 1'
    compare '5 -.5 1'
}

run_tests seq
