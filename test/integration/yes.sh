#!/bin/bash

root="$(dirname "${BASH_SOURCE[0]}")"/../..
name=yes
real=/usr/bin/yes
source "$root"/test/integration/common.sh

test 'empty' \
    '| head -n 50'

test 'string' \
    'hello | head -n 50'

test 'long' \
    '$(cat LICENSE) | head -n 50'

echo "yes tests complete"
