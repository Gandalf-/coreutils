#!/bin/bash

root="$(dirname "${BASH_SOURCE[0]}")"/../..
name=yes
real=/usr/bin/yes
source "$root"/test/integration/common.sh

ptest_empty() {
    compare '| head -n 50'
}

ptest_string() {
    compare 'hello | head -n 50'
}

ptest_long() {
    # BSD yes only takes the first argument so we need quotes
    compare '"$(cat LICENSE)" | head -n 50'
}

run_tests yes
