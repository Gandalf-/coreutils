#!/bin/bash

root="$(dirname "${BASH_SOURCE[0]}")"/../..
source "$root"/test/integration/common.sh

for t in "$root"/test/integration/*.sh; do
    case $t in
        *all.sh|*common.sh) ;;
        *)
            SKIP_REPLAY=1 bash "$t"
        ;;
    esac
done
