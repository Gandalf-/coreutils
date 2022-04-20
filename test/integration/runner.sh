#!/bin/bash

root="$(dirname "${BASH_SOURCE[0]}")"/../..
source "$root"/test/integration/common.sh

for t in "$root"/test/integration/*.sh; do
    case $t in
        *runner.sh|*common.sh) ;;
        *)
            echo -n "running $( basename $t )...\t"
            bash "$t"
        ;;
    esac
done
