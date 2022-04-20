#!/bin/bash

root="$(dirname "${BASH_SOURCE[0]}")"/../..
source "$root"/test/integration/common.sh

f="file.txt"
g="file.jpg"
h="file.png"

function cleanup() { rm -f "$f" "$g" "$h"; }
trap cleanup EXIT

sponge() {
    stack exec -- utils sponge "$@" ||
        die "sponge exited with $?"
}

# help
sponge -h | expect "^sponge:"
sponge --help | expect "^sponge:"


# writing to stdout
echo hello > "$f"
sponge < "$f" | expect hello


# writing to a file
echo hello > "$f"
sponge < "$f" "$f"
expect-file "$f" hello


# writing to a multiple files
echo hello > "$f"
sponge < "$f" "$f" "$g" "$h"
expect-file "$f" hello
expect-file "$g" hello
expect-file "$h" hello


# pipeline
echo hello > "$f"
sed -e 's/l/o/g' "$f" | sponge "$f"
expect-file "$f" heooo


# multiline pipeline
cat > "$f" << EOF
apple
sauce
blue
berry
EOF
sed -e 's/sauce/pie/g' "$f" | grep 'blue' | sponge "$f"
expect-file "$f" blue


# binary pipeline
head -c 100 /dev/urandom > "$f"
before="$( sha256sum "$f" )"

sponge < "$f" "$f"

after="$( sha256sum "$f" )"
[[ "$before" == $after ]] ||
    die "$f corrupted writing binary data to itself"


# large binary pipeline
head -c $(( 5 * 1024 * 1024 )) /dev/urandom > "$f"
before="$( sha256sum "$f" )"

sponge < "$f" "$f"

after="$( sha256sum "$f" )"
[[ "$before" == $after ]] ||
    die "$f corrupted writing binary data to itself"
