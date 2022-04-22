#!/bin/bash

root="$(dirname "${BASH_SOURCE[0]}")"/../..
name=head
real=/usr/bin/head
source "$root"/test/integration/common.sh

ptest_file_bytes() {
    compare -c 10 LICENSE
}

ptest_file_negative_bytes() {
    compare -c -10 LICENSE
}

ptest_file_lines() {
    compare -n 10 LICENSE
}

ptest_file_negative_lines() {
    compare -n -10 LICENSE
}

ptest_file_overflow_bytes() {
    compare -c 10000 LICENSE
}

ptest_file_overflow_negative_bytes() {
    compare -c -10000 LICENSE
}

ptest_file_overflow_lines() {
    compare -n 10000 LICENSE
}

ptest_file_overflow_negative_lines() {
    compare -n -10000 LICENSE
}


ptest_stdin_bytes() {
    compare -c 10 \< LICENSE
}

ptest_stdin_negative_bytes() {
    compare -c -10 \< LICENSE
}

ptest_stdin_lines() {
    compare -n 10 \< LICENSE
}

ptest_stdin_negative_lines() {
    compare -n -10 \< LICENSE
}

ptest_stdin_overflow_bytes() {
    compare -c 10000 \< LICENSE
}

ptest_stdin_overflow_negative_bytes() {
    compare -c -10000 \< LICENSE
}

ptest_stdin_overflow_lines() {
    compare -n 10000 \< LICENSE
}

ptest_stdin_overflow_negative_lines() {
    compare -n -10000 \< LICENSE
}


ptest_multiple_files_lines() {
    compare -n 10 LICENSE README.md stack.yaml
}


run_tests head
