#!/bin/sh
# Freeze what the merge computes, and check it still computes that.
#
#   merge-golden.sh check   compare against the recorded answers
#   merge-golden.sh bless   record the answers afresh
#
# The differential check against bsc's exporter (merge-corpus.sh) is
# scaffolding: it goes when the design-level export goes, and takes
# every design's worth of evidence with it.  What is recorded here was
# vouched for by that check on the day it was written, and afterwards
# it is what notices the merge changing its mind.
#
# A recorded answer catches CHANGE, not error.  Re-blessing a
# disagreement without understanding it throws away the only thing
# these files are for.
#
# BSC_INST overrides where bsc/trs-bir/trs are found (default: inst/).
set -u

here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../../.." && pwd)
bin=${BSC_INST:-$root/inst}/bin
want=$root/src/trs/tests/merge
mode=${1:-check}

[ -x "$bin/trs-bir" ] || { echo "no trs-bir under $bin; build first" >&2; exit 2; }

wk=$(mktemp -d) || exit 2
trap 'rm -rf "$wk"' EXIT

fail=0
checked=0
while read -r rel; do
    case "$rel" in ''|\#*) continue;; esac
    n=$(basename "$rel" .bsv)
    top=$(grep -oE '^module +sys[A-Za-z0-9_]*' "$root/$rel" | head -1 | awk '{print $2}')
    [ -n "$top" ] || { echo "FAIL $n (no sys* top)"; fail=1; continue; }
    d=$wk/$n
    mkdir -p "$d" && cp "$root/$rel" "$d/" || { echo "FAIL $n (copy)"; fail=1; continue; }
    ( cd "$d" \
      && { "$bin/bsc" -sim "$n.bsv" >/dev/null 2>&1 \
           && "$bin/bsc" -sim -e "$top" -o o.out >/dev/null 2>&1 \
           || "$bin/bsc" -sim -sched-dynamic -u -g "$top" "$n.bsv" >/dev/null 2>&1; } \
      && "$bin/trs-bir" "$top" >/dev/null 2>&1 \
      && TRS_MERGE_DUMP="$d/got" "$bin/trs" ir dump "$top.bir" >/dev/null 2>&1 ) \
      || { echo "FAIL $n (build)"; fail=1; continue; }

    checked=$((checked + 1))
    if [ "$mode" = bless ]; then
        cp "$d/got" "$want/$n.expected"
        echo "BLESS $n"
    elif cmp -s "$d/got" "$want/$n.expected" 2>/dev/null; then
        echo "PASS $n"
    else
        echo "FAIL $n (schedule changed)"
        diff "$want/$n.expected" "$d/got" 2>/dev/null | head -8
        fail=1
    fi
done < "$want/designs.txt"

echo "$checked designs"
exit $fail
