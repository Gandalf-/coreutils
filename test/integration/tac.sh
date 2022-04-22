#!/bin/bash

root="$(dirname "${BASH_SOURCE[0]}")"/../..
source "$root"/test/integration/common.sh

f="file.txt"

function cleanup() { rm -f "$f"; }
trap cleanup EXIT

tac() {
    stack exec -- utils tac "$@"
}


test_basic_stdin_tac() {
    cat > "$f" << EOF
a
b
c
EOF
    expect-file "$f" "$( tac < "$f" | tac )" 'basic stdin tac'
}

test_basic_file_tac() {
    cat > "$f" << EOF
a
b
c
EOF
    expect-file "$f" "$( tac "$f" | tac )" 'basic file tac'
}

test_trailing_newline_stdin_tac() {
    cat > "$f" << EOF
a
b
c

EOF
    expect-file "$f" "$( tac < "$f" | tac )" 'trailing newline stdin tac'
}

test_trailing_newline_file_tac() {
    cat > "$f" << EOF
a
b
c

EOF
    expect-file "$f" "$( tac "$f" | tac )" 'trailing newline file tac'
}

test_preceding_newline_stdin_tac() {
    cat > "$f" << EOF

a
b
c
EOF
    expect-file "$f" "$( tac < "$f" | tac )" 'preceding newline stdin tac'
}

test_preceding_newline_file_tac() {
    cat > "$f" << EOF

a
b
c
EOF
    expect-file "$f" "$( tac "$f" | tac )" 'preceding newline file tac'
}

ptest_no_newline_stdin_tac() {
    equal \
    "$( echo -n hello | sha1sum )" \
    "$( echo -n hello | tac | sha1sum )" \
    'no newline stdin tac'
}

test_binary_stdin_tac() {
    a="$( head -c 10 /dev/urandom | tee "$f" | tac | tac | sha1sum | awk '{print $1}' )"
    b="$( sha1sum "$f" | awk '{print $1}' )"

    [[ "$a" == "$b" ]] || die "before $a != after $b"
}


run_tests tac
