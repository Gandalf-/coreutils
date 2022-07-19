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

ptest_multiple_actions() {
    temp t
    echo 'A B
    C D
    E F G' | awk '{print $2, NF}' > $t
    expect-file "$t" 'B 2
D 2
F 3'
}

ptest_comparison() {
    temp t
    echo '1 2 apple
    2 1 blueberry
    3 4 pineapple' | awk '$1 < $2 {print $3}' > $t
    expect-file "$t" 'apple
pineapple'
}

ptest_multiple_comparison() {
    temp t
    echo '1 2 apple
    2 1 blueberry
    3 4 pineapple' | awk '$1 < $2 && /pine/ {print $3}' > $t
    expect-file "$t" 'pineapple'
}

ptest_variables() {
    temp i o
cat << EOF > $i
4
3
2
8
2
EOF
    awk '$1 > x { x = $1 } END { print x }' $i | expect 8
}

ptest_command_line_variables() {
    echo 1 | awk -v x=99 'END { print x }' | expect 99
}

run_tests awk
