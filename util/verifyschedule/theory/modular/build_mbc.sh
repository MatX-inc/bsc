#!/bin/bash
# build_mbc.sh: MkBufferControl again after a header-comment change (the group-width row), appended to build/all.log
cd "$(dirname "$0")"
TB=$HOME/.elan/toolchains/leanprover--lean4---v4.35.0/bin
echo "== (rebuilt after the header row 'fields of one fact group' was made exact) e2e/MkBufferControl.lean $(date +%T) load $(cut -d' ' -f1 /proc/loadavg) avail $(free -g | awk '/Mem/{print $7}')G" >> build/all.log
LEAN_PATH=build /usr/bin/time -f "WALL %e USER %U SYS %S MAXRSS_KB %M" prlimit --as=25769803776 timeout 1800 \
  $TB/lean -j1 -DElab.async=false -o e2e/build/MkBufferControl.olean -i e2e/build/MkBufferControl.ilean e2e/MkBufferControl.lean 2>&1 | grep -v "^warning\|^  \|linter\|^$\|^Note\|^Hint\|warning:" >> build/all.log
echo "rc ${PIPESTATUS[0]}" >> build/all.log
for fr in "" "--fresh"; do
  echo "== leanchecker $fr MkBufferControl (rebuilt)" >> build/all.log
  PATH=$TB:$PATH LEAN_PATH=build:e2e/build /usr/bin/time -f "WALL %e USER %U SYS %S MAXRSS_KB %M" prlimit --as=25769803776 timeout 1800 \
    $TB/leanchecker $fr MkBufferControl >> build/all.log 2>&1
  echo "rc $?" >> build/all.log
done
echo "== done (MkBufferControl) $(date +%T)" >> build/all.log
