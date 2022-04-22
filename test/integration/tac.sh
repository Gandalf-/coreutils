#!/bin/bash

root="$(dirname "${BASH_SOURCE[0]}")"/../..
source "$root"/test/integration/common.sh

f="file.txt"

function cleanup() { rm -f "$f"; }
trap cleanup EXIT

tac() {
    stack exec -- utils tac "$@"
}


# basic stdin tac
cat > "$f" << EOF
a
b
c
EOF
expect-file "$f" "$( tac < "$f" | tac )" 'basic stdin tac'


# basic file tac
cat > "$f" << EOF
a
b
c
EOF
expect-file "$f" "$( tac "$f" | tac )" 'basic file tac'


# trailing newline stdin tac
cat > "$f" << EOF
a
b
c

EOF
expect-file "$f" "$( tac < "$f" | tac )" 'trailing newline stdin tac'


# trailing newline file tac
cat > "$f" << EOF
a
b
c

EOF
expect-file "$f" "$( tac "$f" | tac )" 'trailing newline file tac'


# preceding newline stdin tac
cat > "$f" << EOF

a
b
c
EOF
expect-file "$f" "$( tac < "$f" | tac )" 'preceding newline stdin tac'


# preceding newline file tac
cat > "$f" << EOF

a
b
c
EOF
expect-file "$f" "$( tac "$f" | tac )" 'preceding newline file tac'


# no newline stdin tac
equal \
  "$( echo -n hello | sha1sum )" \
  "$( echo -n hello | tac | sha1sum )" \
  'no newline stdin tac'


# binary stdin tac
a="$( head -c 10 /dev/urandom | tee "$f" | tac | tac | sha1sum | awk '{print $1}' )"
b="$( sha1sum "$f" | awk '{print $1}' )"

[[ "$a" == "$b" ]] || die "before $a != after $b"


echo "tac tests complete"
