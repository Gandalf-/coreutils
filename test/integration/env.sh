#!/bin/bash

root="$(dirname "${BASH_SOURCE[0]}")"/../..
source "$root"/test/integration/common.sh

env() {
    stack exec -- utils env "$@" ||
        die "env unexpected exited with $?"
}


ptest_empty_environment() {
    env - | expect-empty
    env -i | expect-empty
    env --ignore-environment | expect-empty
}

ptest_variable_reporting() {
    APPLE=1 env | expect APPLE=1
}

ptest_variable_setting() {
    env APPLE=2 | expect APPLE=2
}

ptest_unset_variable() {
    APPLE=1 env -u APPLE | expect-not APPLE
    APPLE=1 env --unset APPLE | expect-not APPLE
    APPLE=1 env APPLE= | expect-not APPLE
}

ptest_execute_command() {
    env sh -c 'echo hello' | expect hello
}

ptest_change_working_directory() {
    env sh -c 'pwd' \
        | expect-not 'test/integration'

    env -C test/integration sh -c 'pwd' \
        | expect 'test/integration'

    env --chdir test/integration sh -c 'pwd' \
        | expect 'test/integration'
}

ptest_multiple_variables() {
    temp f
    env -i APPLE=1 BLUEBERRY=2 > "$f"
    expect-file "$f" "APPLE=1
BLUEBERRY=2"
}

ptest_variables_passed_to_subprocess() {
    env APPLE=SAUCE sh -c 'echo $APPLE' | expect SAUCE
}

# null output not supported
# newline="$( echo APPLE=1 | sha1sum )"
# nulldel="$( echo -n APPLE=1 | sha1sum )"
# out="$( env -i -0 APPLE=1 )"
#
# [[ "$newline" != "$nulldel" ]] || die "test assumptions are wrong"
# [[ "$out" == "$nulldel" ]] || die "output wasn't null delimited"

run_tests env
