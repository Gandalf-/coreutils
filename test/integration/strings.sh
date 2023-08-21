#!/bin/bash

root="$(dirname "${BASH_SOURCE[0]}")"/../..
name=strings
real=/usr/bin/strings
source "$root"/test/integration/common.sh

ptest_ascii() {
    compare 'LICENSE'
    compare 'Coreutils/Nl.hs'
    compare 'Coreutils/Awk.hs'
}

ptest_binary_stdin() {
    temp f
    head -c 1024 /dev/urandom > "$f"
    compare "< $f"
}

ptest_binary_stdin_length() {
    temp f
    head -c 1024 /dev/urandom > "$f"
    compare "-10 < $f"
}

ptest_binary() {
    temp f
    head -c 1024 /dev/urandom > "$f"
    compare "$f"
}

ptest_binary_length() {
    temp f
    head -c 1024 /dev/urandom > "$f"
    compare "-10 $f"
}

run_tests strings
