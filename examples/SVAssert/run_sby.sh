#!/bin/sh
# Bounded model checking of Arb.bs (PASS) or ArbBad.bs (FAIL with a trace)
# with SymbiYosys: needs bsc, yosys, sby and z3 on PATH.
#   ./run_sby.sh mkArb      ./run_sby.sh mkArbBad
set -e
H=$(cd "$(dirname "$0")" && pwd)
M=${1:-mkArb}; MODE=${2:-bmc}
case $M in mkArb) PKG=Arb;; mkArbBad) PKG=ArbBad;; *) echo "usage: $0 mkArb|mkArbBad [bmc|prove]"; exit 1;; esac
W=$H/work_$M; rm -rf "$W"; mkdir -p "$W/v"
bsc -verilog -sva-immediate -u -bdir "$W" -vdir "$W/v" "$H/$PKG.bs"
sed "s/\`DUT/$M/" "$H/formal_top.sv" > "$W/v/formal_top.sv"
cd "$W"
{
  echo "[options]"; echo "mode $MODE"; echo "depth 12"
  echo "[engines]"; echo "smtbmc z3"
  echo "[script]"
  for f in v/*.v v/*.sv; do echo "read -formal $(basename "$f")"; done
  echo "prep -top formal_top"
  echo "[files]"
  for f in v/*.v v/*.sv; do echo "$f"; done
} > "$M.sby"
sby -f "$M.sby"
