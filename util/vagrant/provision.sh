#!/usr/bin/env bash

set -euo pipefail

apt-get update
apt-get install -y \
    autoconf \
    bison \
    build-essential \
    curl \
    flex \
    git \
    gperf \
    iverilog \
    tcl-dev

# GHC and cabal-install come from ghcup (the distro versions may be too
# old); build-type: Hooks needs cabal-install 3.14+
export BOOTSTRAP_HASKELL_NONINTERACTIVE=1
export BOOTSTRAP_HASKELL_GHC_VERSION=9.6.7
export BOOTSTRAP_HASKELL_CABAL_VERSION=latest
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
