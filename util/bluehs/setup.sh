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
# The library links the solvers by name against the installed copies,
# inst/lib/SAT, which `make install-solvers` builds from the vendored
# sources, with that directory named in LIBRARY_PATH; the cabal hooks
# generate BuildVersion.hs (doc/cabal.md section 3).  The build is the whole
# project: the package `bsc` and its programs.

set -e
REPO=$(dirname "$(dirname "$(dirname "$(readlink -f "$0")")")")
cd "$REPO"

make install-solvers
LIBRARY_PATH="$REPO/inst/lib/SAT${LIBRARY_PATH:+:$LIBRARY_PATH}" cabal build
echo "setup.sh: done - try: util/bluehs/bin/dumpbo inst/lib/Libraries/Arbiter.bo"
