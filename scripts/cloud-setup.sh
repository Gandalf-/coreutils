#!/bin/bash
# Claude Code cloud environment setup for Gandalf-/coreutils (Haskell, stack).
# Runs once, then the resulting filesystem is snapshotted and reused (~7 days).
# Sources: GitHub releases + Docker Hub, since downloads.haskell.org and
# stackage.org are not on the default network allowlist.
set -euo pipefail

GHC_VERSION=9.10.3          # must match the compiler of the resolver in stack.yaml
RESOLVER=lts-24.24
STACK_VERSION=3.7.1
HLINT_VERSION=3.10
STYLISH_VERSION=0.15.1.0

export LANG=C.UTF-8 LC_ALL=C.UTF-8
work=$(mktemp -d)
cd "$work"

# --- apt: native libs GHC links against (in background) ---
# `apt-get update` can fail on third-party PPAs in the base image that the
# network policy blocks; the Ubuntu archive still refreshes, so ignore that.
(
    missing=$(for p in libgmp-dev libnuma-dev zlib1g-dev libffi-dev; do
        dpkg -s "$p" > /dev/null 2>&1 || echo "$p"
    done)
    if [ -n "$missing" ]; then
        apt-get update -qq || true
        apt-get install -y -qq $missing
    fi
) > apt.log 2>&1 &
apt_pid=$!

# --- stack, hlint, stylish-haskell: prebuilt GitHub release binaries ---
curl -fsSL "https://github.com/commercialhaskell/stack/releases/download/v$STACK_VERSION/stack-$STACK_VERSION-linux-x86_64.tar.gz" \
    | tar xz --strip-components=1 -C /usr/local/bin "stack-$STACK_VERSION-linux-x86_64/stack"
curl -fsSL "https://github.com/ndmitchell/hlint/releases/download/v$HLINT_VERSION/hlint-$HLINT_VERSION-x86_64-linux.tar.gz" \
    | tar xz -C /opt && ln -sf "/opt/hlint-$HLINT_VERSION/hlint" /usr/local/bin/hlint
curl -fsSL "https://github.com/haskell/stylish-haskell/releases/download/v$STYLISH_VERSION/stylish-haskell-v$STYLISH_VERSION-linux-x86_64.tar.gz" \
    | tar xz --strip-components=1 -C /usr/local/bin "stylish-haskell-v$STYLISH_VERSION-linux-x86_64/stylish-haskell"

# --- GHC: copy /opt/ghc out of the official haskell Docker image ---
image=haskell tag="$GHC_VERSION-slim-bookworm"
token=$(curl -fsSL "https://auth.docker.io/token?service=registry.docker.io&scope=repository:library/$image:pull" \
    | python3 -c 'import sys,json; print(json.load(sys.stdin)["token"])')
reg() { curl -fsSL -H "Authorization: Bearer $token" "$@"; }
digest=$(reg -H "Accept: application/vnd.oci.image.index.v1+json,application/vnd.docker.distribution.manifest.list.v2+json" \
    "https://registry-1.docker.io/v2/library/$image/manifests/$tag" \
    | python3 -c 'import sys,json; print(next(m["digest"] for m in json.load(sys.stdin)["manifests"] if m["platform"]["architecture"]=="amd64"))')
reg -H "Accept: application/vnd.oci.image.manifest.v1+json,application/vnd.docker.distribution.manifest.v2+json" \
    "https://registry-1.docker.io/v2/library/$image/manifests/$digest" \
    | python3 -c 'import sys,json; [print(l["digest"]) for l in json.load(sys.stdin)["layers"]]' > layers
mkdir rootfs
while read -r layer; do
    reg "https://registry-1.docker.io/v2/library/$image/blobs/$layer" | tar xz -C rootfs --wildcards 'opt/ghc/*' 2>/dev/null || true
done < layers
rm -rf /opt/ghc && mv rootfs/opt/ghc /opt/ghc
ln -sf /opt/ghc/"$GHC_VERSION"/bin/* /usr/local/bin/
ghc --version

# --- stack: use the system GHC, never try to download one ---
mkdir -p ~/.stack
cat > ~/.stack/config.yaml <<'EOF'
system-ghc: true
install-ghc: false
compiler-check: newer-minor
casa:
  enable: false
EOF

wait "$apt_pid" || { cat apt.log; exit 1; }

# --- pre-build the project's dependencies into ~/.stack/snapshots ---
# A throwaway project with the same resolver + deps, so this works without the repo.
mkdir warm && cd warm
cat > stack.yaml <<EOF
resolver: $RESOLVER
packages: [.]
EOF
cat > package.yaml <<'EOF'
name: warm
dependencies: [base, array, bytestring, containers, directory, extra, mtl, network,
  parsec, process, random-shuffle, random, regex-tdfa, resourcet, split,
  streaming-bytestring, streaming, text, time, unordered-containers, word8,
  hspec, QuickCheck, silently, temporary]
library: {}
build-tools: [hspec-discover]
EOF
stack build --only-dependencies

cd / && rm -rf "$work"
echo "coreutils cloud setup done"
