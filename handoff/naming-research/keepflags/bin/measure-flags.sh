#!/bin/bash
# usage: EXTRA="<bsc flags>" measure-flags.sh <inst> <outdir>
# copy of harness/isimplify/measure.sh that adds $EXTRA to the bsc command line
# (before the source file); output layout is the same, so summarize.sh and
# compare.sh work on it
INST="$1"; OUT="$2"
SRC=/home/ravi/claude-tmp/claude-1548697660/-home-ravi/6640a2f0-3ba9-46af-8f63-2f02110094cd/scratchpad/isimp/fixtures
mkdir -p "$OUT"
for f in Chain8 Mux Case Chain512 ChainT800 Regs500 Lut256 SChain512 DChain256; do
  d="$OUT/$f"; [ -e "$d" ] && { echo "$d exists; use a fresh outdir"; exit 1; }; mkdir -p "$d"; cp "$SRC/$f.bs" "$d/"
  ( cd "$d" && env -u GHCRTS /usr/bin/time -f "user=%U sys=%S elapsed=%e maxrss_kb=%M" -o time.txt \
      "$INST/bin/bsc" -u -verilog -elab -bdir . -vdir . -info-dir . $EXTRA +RTS -s -RTS "$f.bs" > bsc.out 2> rts.txt; echo "exit=$?" >> bsc.out )
done
