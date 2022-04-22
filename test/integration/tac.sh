#!/bin/bash

root="$(dirname "${BASH_SOURCE[0]}")"/../..
source "$root"/test/integration/common.sh


tac() {
    stack exec -- utils tac "$@"
}


ptest_basic_stdin_tac() {
    temp f
    cat > "$f" << EOF
a
b
c
EOF
    expect-file "$f" "$( tac < "$f" | tac )" 'basic stdin tac'
}

ptest_basic_file_tac() {
    temp f
    cat > "$f" << EOF
a
b
c
EOF
    expect-file "$f" "$( tac "$f" | tac )" 'basic file tac'
}

ptest_trailing_newline_stdin_tac() {
    temp f
    cat > "$f" << EOF
a
b
c

EOF
    expect-file "$f" "$( tac < "$f" | tac )" 'trailing newline stdin tac'
}

ptest_trailing_newline_file_tac() {
    temp f
    cat > "$f" << EOF
a
b
c

EOF
    expect-file "$f" "$( tac "$f" | tac )" 'trailing newline file tac'
}

ptest_preceding_newline_stdin_tac() {
    temp f
    cat > "$f" << EOF

a
b
c
EOF
    expect-file "$f" "$( tac < "$f" | tac )" 'preceding newline stdin tac'
}

ptest_preceding_newline_file_tac() {
    temp f
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

ptest_binary_stdin_tac() {
    temp f
    a="$( head -c 10 /dev/urandom | tee "$f" | tac | tac | sha1sum | awk '{print $1}' )"
    b="$( sha1sum "$f" | awk '{print $1}' )"

    [[ "$a" == "$b" ]] || die "before $a != after $b"
}


run_tests tac
