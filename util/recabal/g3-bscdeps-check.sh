#!/usr/bin/env bash
# G3 for the recabalization: run bscdeps in src/Libraries/Base3-Contexts with
# that directory's make flags and compare its pkg/imp/probe lines with the
# files bsc -u actually opens or probes there (strace). Run from the repo root
# after G2 (needs the built Base1/Base2 .bo files in build/bsvlib).
set -u
TOP="$(cd "$(dirname "$0")/../.." && pwd)"; cd "$TOP"
STAMP="$(date -u +%Y%m%dT%H%M%SZ)"; OUT="${OUT:-$TOP/g3-results/$STAMP}"; mkdir -p "$OUT"
die() { echo "g3: $*" >&2; exit 2; }
BSC="$(cabal list-bin bsc:exe:bsc)"; DEPS="$(cabal list-bin bsc:exe:bscdeps)"; [ -x "$BSC" ] && [ -x "$DEPS" ] || die "need cabal-built bsc and bscdeps"
command -v strace >/dev/null || die "strace not installed"
BUILDDIR="$TOP/build/bsvlib"; [ -d "$BUILDDIR" ] || die "build/bsvlib missing (run G2 first)"
cd src/Libraries/Base3-Contexts
FLAGS=(-stdlib-names -bdir "$BUILDDIR" -p . -vsearch "$BUILDDIR")
"$DEPS" "${FLAGS[@]}" Contexts.bsv > "$OUT/bscdeps.tsv" 2> "$OUT/bscdeps.err"; echo "bscdeps: exit $? ($(wc -l < "$OUT/bscdeps.tsv") lines)"
# what bsc -u touches: every path it stats/opens (successfully or not), relative to this dir where possible
strace -f -e trace=openat,stat,newfstatat,access,readlink -o "$OUT/strace.raw" "$BSC" -u "${FLAGS[@]}" Contexts.bsv > "$OUT/bsc-u.log" 2>&1; echo "bsc -u: exit $?"
grep -o -E '"[^"]+"' "$OUT/strace.raw" | tr -d '"' | grep -E '\.(bsv|bs|bo|ba|defines)$' | sort -u > "$OUT/bsc-touched.txt"
awk -F'\t' '$1=="pkg"{print $4} $1=="probe"{print $3}' "$OUT/bscdeps.tsv" | sort -u > "$OUT/bscdeps-paths.txt"
echo "bscdeps names $(wc -l < "$OUT/bscdeps-paths.txt") paths (pkg resolutions + probes); bsc -u touched $(wc -l < "$OUT/bsc-touched.txt") source/binary paths"
echo "--- resolved pkg files not touched by bsc -u (should be empty):"; comm -23 <(awk -F'\t' '$1=="pkg"{print $4}' "$OUT/bscdeps.tsv" | sort -u) "$OUT/bsc-touched.txt" | tee "$OUT/pkg-not-touched.txt"
echo "--- probes bsc -u never made (candidate order drift; informational):"; comm -23 <(awk -F'\t' '$1=="probe"{print $3}' "$OUT/bscdeps.tsv" | sort -u) "$OUT/bsc-touched.txt" | head -20
echo "--- files bsc -u touched that bscdeps never mentions (should be only non-package files):"; comm -13 "$OUT/bscdeps-paths.txt" "$OUT/bsc-touched.txt" | head -20
echo "imp lines: $(awk -F'\t' '$1=="imp"' "$OUT/bscdeps.tsv" | wc -l); details in $OUT"
