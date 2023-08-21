root="$(dirname "${BASH_SOURCE[0]}")"/../..
name=paste
real=/usr/bin/paste
source "$root"/test/integration/common.sh

ptest_single_file() {
    compare 'LICENSE'
}

ptest_multiple_files() {
    compare 'LICENSE' 'LICENSE'
}

ptest_stdin() {
    compare '- < LICENSE'
}

ptest_multiple_stdin() {
    compare '- - < LICENSE'
}

ptest_mixed() {
    compare '- LICENSE - < LICENSE'
}

ptest_different_lengths() {
    compare 'LICENSE Makefile'
}

ptest_delimiter() {
    compare '-d, LICENSE LICENSE'
}

ptest_multiple_delimiters() {
    compare '-d 12456789 LICENSE LICENSE LICENSE'
}

ptest_serialize() {
    compare '-s LICENSE LICENSE LICENSE'
}

ptest_serialize_delimiter() {
    compare '-d 1234 -s LICENSE LICENSE LICENSE'
}

run_tests paste
