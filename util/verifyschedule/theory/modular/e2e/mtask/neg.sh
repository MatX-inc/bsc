#!/bin/bash
# neg.sh: MethTaskBadEn.lean (MethTask.lean with the rule's task statement enabled always) must fail;
# logged to neg.log with the first error.
cd "$(dirname "$0")/../.."
L=$HOME/.elan/toolchains/leanprover--lean4---v4.35.0/bin/lean
LEAN_PATH=build /usr/bin/time -f "WALL %e USER %U SYS %S MAXRSS_KB %M" prlimit --as=25769803776 timeout 600 \
  $L -j1 -DElab.async=false e2e/mtask/MethTaskBadEn.lean > e2e/mtask/neg.full 2>&1
rc=$?
{ echo "rc $rc (expected 1)"; grep -m1 -A0 "error" e2e/mtask/neg.full; grep "axioms\|WALL" e2e/mtask/neg.full; } > e2e/mtask/neg.log
