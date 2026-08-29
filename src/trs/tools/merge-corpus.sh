#!/bin/sh
# Run the schedule-merge oracle over as much of the bsc testsuite as
# links, and report where the computed schedule disagrees with the one
# the exporter still writes.
#
#   merge-corpus.sh <workdir> [<list-file>]
#
# With no list, every testsuite .bsv declaring a sys* top is tried.
# Designs that do not compile or link are skipped: the testsuite is full
# of error tests and tests needing flags we do not pass, and those are
# not failures of this check.
#
# BSC_INST overrides where bsc/trs-bir/trs are found (default: the
# repo's inst/).  Output lands in <workdir>/diff.txt; each line is a
# disagreement, "ok N" is a design whose N compositions matched, and
# "ok vacuous" is one with no compositions to compare -- counted apart
# because such a design matches whatever the merge does.
set -u

here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../../.." && pwd)
bin=${BSC_INST:-$root/inst}/bin
[ -x "$bin/trs-bir" ] || { echo "no trs-bir under $bin; build first" >&2; exit 2; }

wk=$1
list=${2:-}
mkdir -p "$wk" || exit 2
wk=$(cd "$wk" && pwd)          # absolute: the loop cds into each design
diff=$wk/diff.txt
: > "$diff"

if [ -z "$list" ]; then
    list=$wk/candidates.txt
    ( cd "$root" && grep -rl --include='*.bsv' -E '^module +sys' testsuite ) > "$list"
fi

linked=0
skipped=0
while read -r rel; do
    n=$(basename "$rel" .bsv)
    top=$(grep -oE '^module +sys[A-Za-z0-9_]*' "$root/$rel" | head -1 | awk '{print $2}')
    [ -n "$top" ] || { skipped=$((skipped + 1)); continue; }
    d=$wk/$(echo "$rel" | tr / _)
    mkdir -p "$d" && cp "$root/$rel" "$d/" || { skipped=$((skipped + 1)); continue; }
    if ( cd "$d" \
         && "$bin/bsc" -sim "$n.bsv" >/dev/null 2>&1 \
         && "$bin/bsc" -sim -e "$top" -o o.out >/dev/null 2>&1 \
         && "$bin/trs-bir" "$top" >/dev/null 2>&1 \
         && TRS_MERGE_CHECK="$diff" "$bin/trs" ir dump "$top.bir" >/dev/null 2>&1 )
    then linked=$((linked + 1))
    else skipped=$((skipped + 1))
    fi
done < "$list"

# grep -c exits nonzero on no matches, so count with awk instead
tally() { awk "$1"' { n++ } END { print n + 0 }' "$diff"; }

echo "linked=$linked skipped=$skipped"
echo "matched=$(tally '/^ok [0-9]/')" \
     "vacuous=$(tally '/^ok vacuous/')" \
     "disagreed=$(tally '!/^ok/')"
