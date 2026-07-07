#!/bin/bash
# SessionStart hook: prepare a Claude Code on the web container for bsc +
# TRS development.  Installs the Haskell toolchain and libraries the
# bsc build needs, the LLVM 18 dev packages the trs-codegen crate needs,
# initializes the yices submodule, and warms the Rust workspace.
#
# Deliberately NOT done here: the full bsc build (`make install-src`,
# ~30-60 min) — run it on demand.  Most day-to-day work is on the Rust
# side, where only cargo (crates.io) is needed.
set -euo pipefail

# Only needed in remote (web) containers; local checkouts manage their own env.
if [ "${CLAUDE_CODE_REMOTE:-}" != "true" ]; then
  exit 0
fi

cd "$CLAUDE_PROJECT_DIR"

export DEBIAN_FRONTEND=noninteractive

# --- Haskell toolchain + libraries for the bsc build (INSTALL.md set, plus
# --- cborg/serialise for the BIR exporter, SimExportIR.hs)
apt-get install -y -qq \
  build-essential tcl-dev libgmp-dev pkg-config autoconf gperf flex bison \
  ghc cabal-install \
  libghc-regex-compat-dev libghc-syb-dev libghc-old-time-dev \
  libghc-split-dev libghc-serialise-dev \
  >/dev/null

# --- strict-concurrency (in bsc's PACKAGES list) is not packaged by Ubuntu
# --- and cabal's index protocol is blocked by the proxy; fetch the tarball
# --- directly (plain HTTPS to hackage works) and install locally.
if ! ghc-pkg list 2>/dev/null | grep -q strict-concurrency; then
  tmp=$(mktemp -d)
  curl -sSL -o "$tmp/sc.tar.gz" \
    https://hackage.haskell.org/package/strict-concurrency-0.2.4.3/strict-concurrency-0.2.4.3.tar.gz
  tar -xzf "$tmp/sc.tar.gz" -C "$tmp"
  (cd "$tmp"/strict-concurrency-* && cabal v1-install --global >/dev/null)
  rm -rf "$tmp"
fi

# --- LLVM 18 + zstd for trs-codegen (feature "llvm")
apt-get install -y -qq llvm-18-dev libpolly-18-dev libzstd-dev >/dev/null

# --- yices submodule (needed by the bsc build)
git submodule update --init --recursive >/dev/null 2>&1 || true

# --- Warm the Rust workspace (build + test caches)
if command -v cargo >/dev/null; then
  (cd src/trs && cargo build -q && cargo test -q --no-run) || true
fi

echo "bsc/trs session setup complete"
echo "  full bsc build (when needed): make -C \$CLAUDE_PROJECT_DIR install-src   (~30-60 min)"
