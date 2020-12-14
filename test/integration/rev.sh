#!/bin/bash

root="$(dirname "${BASH_SOURCE[0]}")"/../..
name=rev
real=/usr/bin/rev
source "$root"/test/integration/common.sh

test 'single file' \
    'LICENSE'

test 'multiple files' \
    'LICENSE stack.yaml'

test 'stdin' \
    '< LICENSE'

skip "GNU rev doesn't support this" \
test 'stdin dash' \
    '- < LICENSE'
