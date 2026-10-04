#!/usr/bin/env bash
# G2 for the recabalization: build and install src/Libraries with the
# cabal-built bsc, bound explicitly, and compare the installed inventory and
# content against the make-built oracle. Run from the repository root.
#   ORACLE=/path/to/oracle/inst  (default ../bsc-oracle/inst)
#   OUT=results dir              (default g2-results/<timestamp>)
# bsc stores source paths in .bo and foreign metadata, so the build here
# passes BSCFLAGS='-remap-path-prefix <this tree>=/TREE' and the content
# report is about the compiler only when the oracle's libraries were built
# with the same flag for their own tree (util/engine/p1-check.sh has the
# recipe and the reasons); whether they were is reported, not required.
set -u
TOP="$(cd "$(dirname "$0")/../.." && pwd -P)"; cd "$TOP"
REMAP="-remap-path-prefix $TOP=/TREE"
ORACLE="${ORACLE:-$TOP/../bsc-oracle/inst}"
STAMP="$(date -u +%Y%m%dT%H%M%SZ)"; OUT="${OUT:-$TOP/g2-results/$STAMP}"; mkdir -p "$OUT"
PREFIX="$OUT/inst"
die() { echo "g2: $*" >&2; exit 2; }
BSC="$(cabal list-bin bsc:exe:bsc 2>/dev/null)"; [ -x "$BSC" ] || die "cabal-built bsc not found (cabal build bsc:exe:bsc first)"
[ -x "$ORACLE/bin/tconcheck" ] && [ -x "$ORACLE/bin/bo2bloogle" ] || die "oracle tools missing under $ORACLE/bin (tconcheck, bo2bloogle are make-built)"
[ -d "$ORACLE/lib/Libraries" ] || die "oracle Libraries missing under $ORACLE/lib/Libraries"
{ echo "bsc: $BSC"; sha256sum "$BSC"; echo "tconcheck/bo2bloogle: $ORACLE/bin (make-built oracle tools)"; sha256sum "$ORACLE/bin/tconcheck" "$ORACLE/bin/bo2bloogle"; "$BSC" -v 2>&1 | head -3; echo "BSCFLAGS: $REMAP"; } > "$OUT/provenance.txt"
rm -rf build/bsvlib
t0=$(date +%s)
make -C src/Libraries build install BSC="$BSC" TCONCHECK="$ORACLE/bin/tconcheck" BO2BLOOGLE="$ORACLE/bin/bo2bloogle" PREFIX="$PREFIX" BSCFLAGS="$REMAP" > "$OUT/make.log" 2>&1; rc=$?
echo "make -C src/Libraries build install: exit $rc, $(( $(date +%s) - t0 )) s (log: $OUT/make.log)"; [ $rc -eq 0 ] || { tail -30 "$OUT/make.log"; die "library build failed"; }
inv() { (cd "$1" && find . -type f | sort); }
inv "$ORACLE/lib/Libraries" > "$OUT/oracle-inventory.txt"; inv "$PREFIX/lib/Libraries" > "$OUT/cabal-inventory.txt"
if diff -u "$OUT/oracle-inventory.txt" "$OUT/cabal-inventory.txt" > "$OUT/inventory.diff"; then echo "inventory: IDENTICAL ($(wc -l < "$OUT/cabal-inventory.txt") files)"; inv_ok=1; else echo "inventory: DIFFERS"; cat "$OUT/inventory.diff"; inv_ok=0; fi
# Content: report metadata with an embedded compiler version separately.
if grep -a -q -F '/TREE/src/Libraries/Base1/Prelude.bs' "$ORACLE/lib/Libraries/Prelude.bo"; then echo "oracle paths: remapped to /TREE (content comparable)"; else echo "oracle paths: NOT remapped (its libraries were built without -remap-path-prefix <its tree>=/TREE); the content report includes the path-induced differences"; fi
same=0; diff_other=0; diff_versioned=0; : > "$OUT/content-diffs.txt"
while read -r f; do
  if cmp -s "$ORACLE/lib/Libraries/$f" "$PREFIX/lib/Libraries/$f"; then same=$((same+1)); else echo "$f" >> "$OUT/content-diffs.txt"; case "$f" in *.bdpi|*.bmod|*.bsched|*.ba) diff_versioned=$((diff_versioned+1));; *) diff_other=$((diff_other+1));; esac; fi
done < "$OUT/cabal-inventory.txt"
echo "content: $same identical, $diff_other other files differ, $diff_versioned versioned metadata files differ (list: $OUT/content-diffs.txt)"
echo "result: $([ $inv_ok -eq 1 ] && echo 'LAYOUT PASS' || echo 'LAYOUT FAIL'); content differences are reported, not judged, by this script"
