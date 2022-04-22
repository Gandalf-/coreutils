#!/bin/bash

root="$(dirname "${BASH_SOURCE[0]}")"/../..
source "$root"/test/integration/common.sh

sponge() {
    stack exec -- utils sponge "$@" ||
        die "sponge exited with $?"
}


ptest_help() {
    sponge -h | expect "^sponge:"
    sponge --help | expect "^sponge:"
}

ptest_writing_to_stdout() {
    temp f
    echo hello > "$f"
    sponge < "$f" | expect hello
}

ptest_writing_to_a_file() {
    temp f
    echo hello > "$f"
    sponge < "$f" "$f"
    expect-file "$f" hello
}

ptest_writing_to_multiple_files() {
    temp f g h
    echo hello > "$f"
    sponge < "$f" "$f" "$g" "$h"
    expect-file "$f" hello
    expect-file "$g" hello
    expect-file "$h" hello
}

ptest_pipeline() {
    temp f
    echo hello > "$f"
    sed -e 's/l/o/g' "$f" | sponge "$f"
    expect-file "$f" heooo
}

ptest_multiline_pipeline() {
    temp f
    cat > "$f" << EOF
apple
sauce
blue
berry
EOF
    sed -e 's/sauce/pie/g' "$f" | grep 'blue' | sponge "$f"
    expect-file "$f" blue
}

ptest_binary_pipeline() {
    temp f
    head -c 100 /dev/urandom > "$f"
    before="$( sha256sum "$f" )"

    sponge < "$f" "$f"

    after="$( sha256sum "$f" )"
    [[ "$before" == $after ]] ||
        die "$f corrupted writing binary data to itself"
}

ptest_large_binary_pipeline() {
    temp f
    head -c $(( 5 * 1024 * 1024 )) /dev/urandom > "$f"
    before="$( sha256sum "$f" )"

    sponge < "$f" "$f"

    after="$( sha256sum "$f" )"
    [[ "$before" == $after ]] ||
        die "$f corrupted writing binary data to itself"
}

run_tests sponge
