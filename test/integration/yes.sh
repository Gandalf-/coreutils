#!/bin/bash

root="$(dirname "${BASH_SOURCE[0]}")"/../..
name=yes
real=/usr/bin/yes
source "$root"/test/integration/common.sh

test_empty() {
    compare '| head -n 50'
}

test_string() {
    compare 'hello | head -n 50'
}

test_long() {
    compare '$(cat LICENSE) | head -n 50'
}

run_tests yes
