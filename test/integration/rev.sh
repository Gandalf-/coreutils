#!/bin/bash

root="$(dirname "${BASH_SOURCE[0]}")"/../..
name=rev
real=/usr/bin/rev
source "$root"/test/integration/common.sh

ptest_single_file() {
    compare 'LICENSE'
}

ptest_multiple_files() {
    compare 'LICENSE stack.yaml'
}

ptest_stdin() {
    compare '< LICENSE'
}

run_tests rev
