#!/bin/bash

root="$(dirname "${BASH_SOURCE[0]}")"/../..
name=strings
real=/usr/bin/strings
export LC_ALL=C
source "$root"/test/integration/common.sh

ptest_ascii() {
    compare 'LICENSE'
    compare 'Coreutils/Nl.hs'
    compare 'Coreutils/Awk.hs'
}

# Binary tests pipe through stdin rather than passing file args. macOS /usr/bin/strings in file
# mode does not respect LC_ALL=C: it includes high bytes (128-255) and form feed as printable
# characters, producing inconsistent results with random data. Stdin mode correctly uses ASCII-only
# printable (32-126).

ptest_binary() {
    temp f
    head -c 1024 /dev/urandom > "$f"
    compare "< $f"
}

ptest_binary_length() {
    temp f
    head -c 1024 /dev/urandom > "$f"
    compare "-10 < $f"
}

run_tests strings
