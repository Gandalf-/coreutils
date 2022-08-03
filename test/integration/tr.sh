#!/bin/bash

root="$(dirname "${BASH_SOURCE[0]}")"/../..
name=tr
real=/usr/bin/tr
source "$root"/test/integration/common.sh

ptest_upper_case() {
    compare '[:lower:] [:upper:] < LICENSE'
}

ptest_no_digits() {
    compare '-d [:digit:] < LICENSE'
}

ptest_no_digits_or_spaces() {
    compare '-d [:digit:][:space:] < LICENSE'
}

ptest_squeeze_repeats() {
    compare '-s abc ddeeff < LICENSE'
}

ptest_truncate_sets() {
    compare '-t 123456789 abc < LICENSE'
}

ptest_no_newlines_or_spaces() {
    compare '-d " \n" < LICENSE'
}

ptest_only_newlines() {
    compare '-c -d \n < LICENSE'
}

run_tests tr
