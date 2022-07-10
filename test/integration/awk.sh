#!/bin/bash

root="$(dirname "${BASH_SOURCE[0]}")"/../..
source "$root"/test/integration/common.sh

awk() {
    stack exec -- utils awk "$@" ||
        die "awk unexpected exited with $?"
}

ptest_record_count() {
    echo 'A B C' | awk '{print NF}' | expect 3
}

ptest_multi_line() {
    temp t
    echo 'A
    B C
    D E F' | awk '{print NF}' > $t
    expect-file "$t" '1
2
3'
}

ptest_grep() {
    equal $(
        echo 'apple
        sauce' | awk '/apple/'
    ) 'apple'
}

ptest_exec() {
    equal $(
        seq 3 | awk '{print "hello"}'
    ) 'hello
hello
hello'
}

ptest_file_program() {
    temp prog t
    echo '/apple/ {print NF}' > "$prog"
cat << EOF > "$t"
blue
apple 1 2 3
berry
EOF
    equal $( awk -f "$prog" "$t" ) 4
}

run_tests awk
