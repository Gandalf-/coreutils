#!/bin/bash

root="$(dirname "${BASH_SOURCE[0]}")"/../..
name=head
real=/usr/bin/head

test() {

    local mine="$(
        eval "timeout 1 stack exec utils -- $name ${@:2}" | md5sum
    )"

    local system="$(
        eval "timeout 1 $real ${@:2}" | md5sum
    )"

    [[ "$mine" == "$system" ]] || {
        echo "failure, $*"
    }
}

cd "$root"

test 'file bytes' \
    -c 10 LICENSE

test 'file negative bytes' \
    -c -10 LICENSE

test 'file lines' \
    -n 10 LICENSE

test 'file negative lines' \
    -n -10 LICENSE

test 'file overflow bytes' \
    -c 10000 LICENSE

test 'file overflow negative bytes' \
    -c -10000 LICENSE

test 'file overflow lines' \
    -n 10000 LICENSE

test 'file overflow negative lines' \
    -n -10000 LICENSE


test 'stdin bytes' \
    -c 10 \< LICENSE

test 'stdin negative bytes' \
    -c -10 \< LICENSE

test 'stdin lines' \
    -n 10 \< LICENSE

test 'stdin negative lines' \
    -n -10 \< LICENSE

test 'stdin overflow bytes' \
    -c 10000 \< LICENSE

test 'stdin overflow negative bytes' \
    -c -10000 \< LICENSE

test 'stdin overflow lines' \
    -n 10000 \< LICENSE

test 'stdin overflow negative lines' \
    -n -10000 \< LICENSE


test 'multiple files lines' \
    -n 10 LICENSE README.md stack.yaml
