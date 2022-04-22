#!/bin/bash

root="$(dirname "${BASH_SOURCE[0]}")"/../..
name=head
real=/usr/bin/head
source "$root"/test/integration/common.sh

test_file_bytes() {
    compare -c 10 LICENSE
}

test_file_negative_bytes() {
    compare -c -10 LICENSE
}

test_file_lines() {
    compare -n 10 LICENSE
}

test_file_negative_lines() {
    compare -n -10 LICENSE
}

test_file_overflow_bytes() {
    compare -c 10000 LICENSE
}

test_file_overflow_negative_bytes() {
    compare -c -10000 LICENSE
}

test_file_overflow_lines() {
    compare -n 10000 LICENSE
}

test_file_overflow_negative_lines() {
    compare -n -10000 LICENSE
}


test_stdin_bytes() {
    compare -c 10 \< LICENSE
}

test_stdin_negative_bytes() {
    compare -c -10 \< LICENSE
}

test_stdin_lines() {
    compare -n 10 \< LICENSE
}

test_stdin_negative_lines() {
    compare -n -10 \< LICENSE
}

test_stdin_overflow_bytes() {
    compare -c 10000 \< LICENSE
}

test_stdin_overflow_negative_bytes() {
    compare -c -10000 \< LICENSE
}

test_stdin_overflow_lines() {
    compare -n 10000 \< LICENSE
}

test_stdin_overflow_negative_lines() {
    compare -n -10000 \< LICENSE
}


test_multiple_files_lines() {
    compare -n 10 LICENSE README.md stack.yaml
}


run_tests head
