#!/bin/sh
# Check that a design assembled from fragments is the design the
# whole-design export describes, over as much of the bsc testsuite as
# links.
#
#   frag-corpus.sh <workdir> [<list-file>]
#
# For each design: export the whole .bir and one fragment per
# synthesized module, link the fragments, and compare the artifact's
# run against the whole-design one.  Running both is the check that
# matters -- a string id translated wrongly during the link shows up as
# a wrong name, a wrong port or a wrong constant, and the two runs
# diverge.
#
# BSC_INST overrides where bsc/trs-bir/trs are found (default: the
# repo's inst/).  Output lands in <workdir>/frag.txt, one line per
# design: "ok", "differ" or the stage that skipped it.
set -u

here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../../.." && pwd)
bin=${BSC_INST:-$root/inst}/bin
[ -x "$bin/trs-bir" ] || { echo "no trs-bir under $bin; build first" >&2; exit 2; }

wk=$1
list=${2:-}
mkdir -p "$wk" || exit 2
wk=$(cd "$wk" && pwd)
log=$wk/frag.txt
: > "$log"

if [ -z "$list" ]; then
    list=$wk/candidates.txt
    ( cd "$root" && grep -rl --include='*.bsv' -E '^module +sys' testsuite ) > "$list"
fi

ok=0
differ=0
skipped=0
while read -r rel; do
    n=$(basename "$rel" .bsv)
    top=$(grep -oE '^module +sys[A-Za-z0-9_]*' "$root/$rel" | head -1 | awk '{print $2}')
    [ -n "$top" ] || { skipped=$((skipped + 1)); continue; }
    d=$wk/$(echo "$rel" | tr / _)
    rm -rf "$d"
    mkdir -p "$d" && cp "$root/$rel" "$d/" || { skipped=$((skipped + 1)); continue; }
    # same fallback as merge-corpus.sh: a design needing dynamic
    # ordering does not compile statically at all
    ( cd "$d" \
      && { "$bin/bsc" -sim "$n.bsv" >/dev/null 2>&1 \
           && "$bin/bsc" -sim -e "$top" -o o.out >/dev/null 2>&1 \
           || "$bin/bsc" -sim -sched-dynamic -u -g "$top" "$n.bsv" \
                >/dev/null 2>&1; } \
      && "$bin/trs-bir" "$top" >/dev/null 2>&1 ) \
      || { skipped=$((skipped + 1)); echo "$rel: no export" >> "$log"; continue; }

    # one fragment per synthesized module in this design, the top last
    # (the order the link wants).  A link run writes one .cxx per
    # module in the hierarchy, which is exactly that set; the .ba files
    # are every module bsc elaborated, which for a source file holding
    # several designs is more.  Only the dynamic-schedule fallback,
    # which does not link, has to fall back to the .ba names.
    mods=$(cd "$d" && ls *.cxx 2>/dev/null | sed 's/\.cxx$//' \
             | grep -v "^model_")
    [ -n "$mods" ] || mods=$(cd "$d" && ls *.ba 2>/dev/null \
                               | sed 's/\.ba$//' | grep -vE "^sys")
    # a name with no fragment is not fatal here: not every .ba is a
    # module (a foreign function gets one too), and a fragment the
    # design really needs is one the link refuses to go without
    frags=""
    for m in $mods; do
        [ "$m" = "$top" ] && continue
        ( cd "$d" && "$bin/trs-bir" --single-fragment -o "$m.frag.bir" "$m" \
            >/dev/null 2>&1 ) || continue
        frags="$frags $m.frag.bir"
    done
    ( cd "$d" && "$bin/trs-bir" --single-fragment -o "$top.frag.bir" "$top" \
        >/dev/null 2>&1 ) \
      || { skipped=$((skipped + 1)); echo "$rel: no top fragment" >> "$log"; continue; }
    frags="$frags $top.frag.bir"

    ( cd "$d" && "$bin/trs" link -o whole.cexe "$top.bir" >/dev/null 2>&1 ) \
      || { skipped=$((skipped + 1)); echo "$rel: whole link failed" >> "$log"; continue; }
    if ! ( cd "$d" && "$bin/trs" link --multi-fragments -o frag.cexe $frags \
             > frag.link 2>&1 )
    then
        differ=$((differ + 1))
        echo "$rel: fragment link failed: $(sed -n 1p "$d/frag.link")" >> "$log"
        continue
    fi
    ( cd "$d" && ./whole.cexe -m 2000 > whole.run 2>&1; ./frag.cexe -m 2000 > frag.run 2>&1 )
    if ( cd "$d" && cmp -s whole.run frag.run ); then
        ok=$((ok + 1))
        echo "$rel: ok" >> "$log"
        rm -rf "$d"
    else
        differ=$((differ + 1))
        echo "$rel: differ" >> "$log"
    fi
done < "$list"

echo "ok=$ok differ=$differ skipped=$skipped"
