#!/bin/bash
# Build the modular theory and its examples in dependency order, capped (24 GB, -j1).
cd "$(dirname "$0")"
TB=${LEAN_TOOLCHAIN:-$HOME/.elan/toolchains/leanprover--lean4---v4.35.0}/bin
mkdir -p build split/build
run() {  # run LEAN_PATH FILE [OUT]
  local opt=""; [ -n "$3" ] && opt="-o $3.olean -i $3.ilean"
  echo "== $2"
  LEAN_PATH=$1:$TB/../lib/lean prlimit --as=25769803776 timeout 1800 $TB/lean -j1 -DElab.async=false $opt $2 || exit 1
}
for f in Modular Fifo2R EmitLemmas Scan SiteCond Example; do run build $f.lean build/$f; done
run build Scale.lean
for f in ExLib ExChildIface ExChild ExChildRealizes ExTop ExTopRealizes; do
  run build:split/build split/$f.lean split/build/$f
done
run build examples/ScanEx.lean build/ScanEx
for f in ScanExLate ScanTasks EqvTest; do run build examples/$f.lean; done
