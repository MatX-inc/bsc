#!/usr/bin/env bash
# G2 for the recabalization: build and install src/Libraries with the
# cabal-built bsc, bound explicitly, and compare the installed inventory and
# content against the make-built oracle. Run from the repository root.
#   ORACLE=/path/to/oracle/inst  (default ../bsc-oracle/inst)
#   OUT=results dir              (default g2-results/<timestamp>)
set -u
TOP="$(cd "$(dirname "$0")/../.." && pwd)"; cd "$TOP"
ORACLE="${ORACLE:-$TOP/../bsc-oracle/inst}"
STAMP="$(date -u +%Y%m%dT%H%M%SZ)"; OUT="${OUT:-$TOP/g2-results/$STAMP}"; mkdir -p "$OUT"
PREFIX="$OUT/inst"
die() { echo "g2: $*" >&2; exit 2; }
BSC="$(cabal list-bin bsc:exe:bsc 2>/dev/null)"; [ -x "$BSC" ] || die "cabal-built bsc not found (cabal build bsc:exe:bsc first)"
[ -x "$ORACLE/bin/tconcheck" ] && [ -x "$ORACLE/bin/bo2bloogle" ] || die "oracle tools missing under $ORACLE/bin (tconcheck, bo2bloogle are make-built)"
[ -d "$ORACLE/lib/Libraries" ] || die "oracle Libraries missing under $ORACLE/lib/Libraries"
{ echo "bsc: $BSC"; sha256sum "$BSC"; echo "tconcheck/bo2bloogle: $ORACLE/bin (make-built oracle tools)"; sha256sum "$ORACLE/bin/tconcheck" "$ORACLE/bin/bo2bloogle"; "$BSC" -v 2>&1 | head -3; } > "$OUT/provenance.txt"
rm -rf build/bsvlib
t0=$(date +%s)
make -C src/Libraries build install BSC="$BSC" TCONCHECK="$ORACLE/bin/tconcheck" BO2BLOOGLE="$ORACLE/bin/bo2bloogle" PREFIX="$PREFIX" > "$OUT/make.log" 2>&1; rc=$?
echo "make -C src/Libraries build install: exit $rc, $(( $(date +%s) - t0 )) s (log: $OUT/make.log)"; [ $rc -eq 0 ] || { tail -30 "$OUT/make.log"; die "library build failed"; }
inv() { (cd "$1" && find . -type f | sort); }
inv "$ORACLE/lib/Libraries" > "$OUT/oracle-inventory.txt"; inv "$PREFIX/lib/Libraries" > "$OUT/cabal-inventory.txt"
if diff -u "$OUT/oracle-inventory.txt" "$OUT/cabal-inventory.txt" > "$OUT/inventory.diff"; then echo "inventory: IDENTICAL ($(wc -l < "$OUT/cabal-inventory.txt") files)"; inv_ok=1; else echo "inventory: DIFFERS"; cat "$OUT/inventory.diff"; inv_ok=0; fi
# content: byte comparison per file; .ba files embed the compiler version string, so report them separately
same=0; diff_bo=0; diff_ba=0; : > "$OUT/content-diffs.txt"
while read -r f; do
  if cmp -s "$ORACLE/lib/Libraries/$f" "$PREFIX/lib/Libraries/$f"; then same=$((same+1)); else echo "$f" >> "$OUT/content-diffs.txt"; case "$f" in *.ba) diff_ba=$((diff_ba+1));; *) diff_bo=$((diff_bo+1));; esac; fi
done < "$OUT/cabal-inventory.txt"
echo "content: $same identical, $diff_bo non-.ba files differ, $diff_ba .ba files differ (list: $OUT/content-diffs.txt)"
echo "result: $([ $inv_ok -eq 1 ] && echo 'LAYOUT PASS' || echo 'LAYOUT FAIL'); content differences are reported, not judged, by this script"
