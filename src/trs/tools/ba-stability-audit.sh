#!/bin/bash
# Boundary-stability audit: measures how bsc's per-package artifacts
# (.bo/.ba) behave under the three conditions that decide Bazel
# cacheability and cross-package cutoff — and, equivalently, how
# load-bearing the synthesize-boundary interface contract is.
#
#   A. same-dir determinism: wipe outputs, recompile in place, N=3 —
#      byte-compare (.bo/.ba must be identical; -no-show-timestamps
#      -no-show-version are load-bearing).
#   B. path sensitivity: identical sources in two differently-named
#      dirs — byte-compare (FAILS without -remap-path-prefix; with
#      PR #1040's flag and "$PWD=." it must PASS).
#   C. boundary leak: a one-constant LEAF BODY edit — the PARENT's
#      .bo/.ba must (eventually) be byte-stable.  Today it FAILS two
#      ways: elaboration heap positions leak into parent def names
#      (__h<N>), and .bo impHashes/ipkg_depends hash the import's
#      FULL content.  This check is the acceptance test for the
#      interface-contract refactor (boundary-stub elaboration +
#      position-independent naming + contract-scoped hashes).
#
# Usage: BSC=/path/bsc ba-stability-audit.sh <workdir> <leaf.bsv> \
#           <top.bsv> <topmod> <leaf-edit-sed-expr> [extra bsc flags]
# Example (grid8 split):
#   ba-stability-audit.sh /tmp/audit Tile.bsv Grid8.bsv sysGrid8 \
#       's/9e3779b9/9e3779ba/' -remap-path-prefix "$PWD=."
# Measured baseline 2026-07-12 (no remap flag): A PASS, B FAIL
# (paths at .bo offset ~21, .ba post-version), C FAIL (parent
# sysGrid8.ba: 12.8k dumpba diff lines from one leaf constant).
set -u
BSC=${BSC:-bsc}
WK=$1; LEAF=$2; TOP=$3; TOPMOD=$4; EDIT=$5; shift 5
EXTRA=("$@")
FLAGS=(-no-show-timestamps -no-show-version "${EXTRA[@]}")
SRC=$(pwd)
rc=0
compile() { (cd "$1" && $BSC "${FLAGS[@]}" -sim -u -g "$TOPMOD" "$TOP" >/dev/null 2>&1); }
sums() { (cd "$1" && sha256sum *.bo *.ba 2>/dev/null | sort -k2); }

mkdir -p "$WK"/same && cp "$LEAF" "$TOP" "$WK"/same/
for i in 1 2 3; do
  (cd "$WK"/same && rm -f *.bo *.ba) && compile "$WK"/same && sums "$WK"/same > "$WK"/same.$i
done
if cmp -s "$WK"/same.1 "$WK"/same.2 && cmp -s "$WK"/same.1 "$WK"/same.3; then
  echo "A same-dir determinism: PASS"
else echo "A same-dir determinism: FAIL"; diff "$WK"/same.{1,2} | head -6; rc=1; fi

mkdir -p "$WK"/pa "$WK"/pb-longer-path-component
cp "$LEAF" "$TOP" "$WK"/pa/; cp "$LEAF" "$TOP" "$WK"/pb-longer-path-component/
compile "$WK"/pa; compile "$WK"/pb-longer-path-component
bfail=0
for f in "$WK"/pa/*.bo "$WK"/pa/*.ba; do
  cmp -s "$f" "$WK"/pb-longer-path-component/"$(basename "$f")" || { bfail=1; echo "  path-sensitive: $(basename "$f")"; }
done
[ $bfail -eq 0 ] && echo "B path insensitivity: PASS" || { echo "B path insensitivity: FAIL"; rc=1; }

mkdir -p "$WK"/cut && cp "$LEAF" "$TOP" "$WK"/cut/
compile "$WK"/cut
(cd "$WK"/cut && for f in *.bo *.ba; do cp "$f" "$f.before"; done)
(cd "$WK"/cut && sed -i "$EDIT" "$LEAF")
compile "$WK"/cut
cfail=0
leafbase=$(basename "$LEAF" .bsv)
for f in "$WK"/cut/*.bo "$WK"/cut/*.ba; do
  b=$(basename "$f")
  case "$b" in *.before) continue;; "$leafbase".bo|mk*.ba) continue;; esac
  cmp -s "$f" "$f.before" || { cfail=1; echo "  boundary leak into: $b"; }
done
[ $cfail -eq 0 ] && echo "C parent stability under leaf body edit: PASS" || { echo "C parent stability under leaf body edit: FAIL (the contract-refactor acceptance test)"; rc=1; }
exit $rc
