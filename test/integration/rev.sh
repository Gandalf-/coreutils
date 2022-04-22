#!/bin/bash

root="$(dirname "${BASH_SOURCE[0]}")"/../..
name=rev
real=/usr/bin/rev
source "$root"/test/integration/common.sh

test_single_file() {
    compare 'LICENSE'
}

test_multiple_files() {
    compare 'LICENSE stack.yaml'
}

test_stdin() {
    compare '< LICENSE'
}

run_tests rev
