#!/bin/bash
# usage: runprof.sh m...   (sequential; section times from @@ markers)
cd "$(dirname "$0")/.."
OUTD=${OUTD:-prof}
L=$HOME/.elan/toolchains/leanprover--lean4---v4.35.0/bin/lean
for m in "$@"; do
  MARK=1 python3 -I gen/mkemit.py $m $OUTD/M$m.lean
  ( LEAN_PATH=build /usr/bin/time -f "TOTAL %e s %U user %M KB" prlimit --as=25769803776 timeout 3000 $L -j1 -DElab.async=false $OUTD/M$m.lean ) > $OUTD/M$m.out 2>&1
  echo "m=$m done: $(tail -1 $OUTD/M$m.out)"
done
