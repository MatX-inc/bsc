#!/bin/sh
# Build the bsc library for script and ghci use (see README.md).
#
# Run this once after cloning, and again whenever the compiler sources
# change or a make build has been done at a new git commit.  The second
# point is a correctness requirement, not hygiene: .bo/.ba files embed the
# build version string (git hash, via BuildVersion.hs), and the library
# refuses .ba files whose stamp differs from its own
# (ABinUtil.decodeABin -> EBinFileVerMismatch).  The library and the bsc
# that produced your .ba files must be built from the same BuildVersion.hs.
#
# The cabal build's own hooks build the vendored solver libraries (left
# under src/vendor/stp/lib and src/vendor/yices/lib) and generate
# BuildVersion.hs, so there is nothing to prepare.  The build is the whole
# project: the component packages under components/, the facade `bsc`
# that re-exports them, and the programs.

set -e
REPO=$(dirname "$(dirname "$(dirname "$(readlink -f "$0")")")")
cd "$REPO"

cabal build
echo "setup.sh: done - try: util/bluehs/bin/dumpbo inst/lib/Libraries/Arbiter.bo"
