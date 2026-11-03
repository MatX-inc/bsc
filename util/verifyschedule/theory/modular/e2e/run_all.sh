#!/bin/bash
# run_all.sh: the e2e with and without task statements, then leanchecker on the full one; one process at a time.
cd "$(dirname "$0")/.."
TB=$HOME/.elan/toolchains/leanprover--lean4---v4.35.0/bin
for f in SysArithPipe SysArithPipeNoTasks; do
  echo "== $f $(uptime)"
  LEAN_PATH=build /usr/bin/time -f "WALL %e USER %U MAXRSS_KB %M" prlimit --as=25769803776 timeout 2400 $TB/lean -j1 -DElab.async=false \
    -o e2e/build/$f.olean -i e2e/build/$f.ilean e2e/$f.lean 2>&1 | grep -v "^warning\|linter\|^Note"
done
ls -la e2e/build/
echo "== leanchecker SysArithPipe $(uptime)"
PATH=$TB:$PATH LEAN_PATH=build:e2e/build /usr/bin/time -f "WALL %e USER %U MAXRSS_KB %M" prlimit --as=25769803776 timeout 2400 $TB/leanchecker SysArithPipe 2>&1 | tail -2
echo "== leanchecker --fresh SysArithPipe $(uptime)"
PATH=$TB:$PATH LEAN_PATH=build:e2e/build /usr/bin/time -f "WALL %e USER %U MAXRSS_KB %M" prlimit --as=25769803776 timeout 2400 $TB/leanchecker --fresh SysArithPipe 2>&1 | tail -2
echo "== done $(uptime)"
