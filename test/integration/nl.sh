#!/bin/bash

root="$(dirname "${BASH_SOURCE[0]}")"/../..
name=nl
real=/usr/bin/nl
source "$root"/test/integration/common.sh

ptest_defaults() {
    compare 'LICENSE'
    compare 'Coreutils/Nl.hs'
    compare 'Coreutils/Awk.hs'
}

ptest_formatting() {
    compare '-nln LICENSE'
    compare '-nrn LICENSE'
    compare '-nrz LICENSE'
}

ptest_separator() {
    compare '-s??? LICENSE'
}

ptest_width() {
    compare '-w 5 LICENSE'
    compare '-w 100 LICENSE'
}

ptest_style() {
    compare '-ba LICENSE'
    compare '-bt LICENSE'
    compare '-bn LICENSE'

    compare '-bpAustin LICENSE'
    compare '-bp, LICENSE'
}

ptest_numbering() {
    compare '-i37 LICENSE'
    compare '-v9999 LICENSE'
    compare '-v9999 -i9999 LICENSE'
}

ptest_option_soup() {
    compare '-i 3489 -v 849 -b a -nrz -w 74 -s HELLO'
}

run_tests nl
