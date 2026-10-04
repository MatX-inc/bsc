#!/usr/bin/env bash
# Standalone source-level regression. Requires the project's Cabal/GHC tools
# in PATH and a built bsc-core package. It does not rebuild the compiler.
set -euo pipefail

fixture_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
repo_dir=$(cd -- "$fixture_dir/../../.." && pwd)
cd "$repo_dir"
python3 "$fixture_dir/audit_fields.py"
exec "${CABAL:-cabal}" exec -- "${GHC:-ghc}" -v0 -ignore-dot-ghci \
    -package bsc-core "$fixture_dir/VerifyPhaseConfig.hs" -e Main.main
