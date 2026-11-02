#!/bin/sh
# Compile each example with bsc and, for every module it synthesizes,
#   * compare the model verifyschedule extracts (--dump-design) with
#     expected/<module>.dump: extraction tested on its own;
#   * check the Lean file verifyschedule writes; and
#   * check that mutants of its hardware fail to check (mutate.py).
#
#   check.sh                       all of examples/
#   check.sh LookupQ.bsv ...       some of them (names or paths)
#
# BSC and LEAN name the tools (default: the checkout's inst/bin/bsc, else bsc
# on PATH; lean on PATH, else ~/.elan/bin/lean).  MUTANTS is the number of
# mutants of each kind (default 2; 0 skips that test, which needs python3).
# UPDATE=1 rewrites the expected dumps instead of comparing them.  The outputs
# go to OUT if it is set, and otherwise to a temporary directory, kept only if
# a check fails.
set -u

HERE=$(cd "$(dirname "$0")" && pwd)
REPO=$(cd "$HERE/../.." && pwd)
VS=${VS:-$REPO/inst/bin/verifyschedule}
EXPECTED=$HERE/examples/expected
MUTANTS=${MUTANTS:-2}
UPDATE=${UPDATE:-0}

if [ -z "${BSC:-}" ]; then
    if [ -x "$REPO/inst/bin/bsc" ]; then BSC=$REPO/inst/bin/bsc; else BSC=bsc; fi
fi
if [ -z "${LEAN:-}" ]; then
    if command -v lean >/dev/null 2>&1; then LEAN=lean; else LEAN=$HOME/.elan/bin/lean; fi
fi
export LEAN
for tool in "$BSC" "$LEAN"; do
    command -v "$tool" >/dev/null 2>&1 || { echo "check.sh: cannot run $tool" >&2; exit 2; }
done

if [ -n "${OUT:-}" ]; then
    mkdir -p "$OUT"; keep=1
else
    OUT=$(mktemp -d "${TMPDIR:-/tmp}/verifyschedule.XXXXXX"); keep=0
fi

[ $# -gt 0 ] || set -- "$HERE"/examples/*.bsv
failed=0
for f in "$@"; do
    case "$f" in
        */*) src=$(cd "$(dirname "$f")" && pwd)/$(basename "$f") ;;
        *)   src=$HERE/examples/$f ;;
    esac
    name=$(basename "$src" .bsv)
    dir=$OUT/$name
    rm -rf "$dir"; mkdir -p "$dir"
    # compiled from its own directory, so positions name the file alone
    if ! (cd "$(dirname "$src")" && "$BSC" -elab -u -verilog -bdir "$dir" -vdir "$dir" \
            -info-dir "$dir" "$name.bsv") > "$dir/bsc.log" 2>&1; then
        echo "FAIL  $name: bsc (see $dir/bsc.log)"; failed=1; continue
    fi
    for ba in "$dir"/*.ba; do
        [ -f "$ba" ] || continue
        mod=$(basename "$ba" .ba)
        # only the modules this file defines
        grep -q "^ *module *$mod\b" "$src" || continue
        what="$name/$mod"
        # extraction
        (cd "$dir" && "$VS" --dump-design "$mod.ba") > "$dir/$mod.dump" 2>&1
        if [ "$UPDATE" = 1 ]; then
            mkdir -p "$EXPECTED"; cp "$dir/$mod.dump" "$EXPECTED/$mod.dump"
        elif [ ! -f "$EXPECTED/$mod.dump" ]; then
            echo "FAIL  $what: no expected/$mod.dump (UPDATE=1 writes it)"; failed=1; continue
        elif ! diff -u "$EXPECTED/$mod.dump" "$dir/$mod.dump" > "$dir/$mod.dump.diff"; then
            echo "FAIL  $what: extraction differs (see $dir/$mod.dump.diff)"; failed=1; continue
        fi
        # the proof
        if ! (cd "$dir" && "$VS" "$mod.ba" > "$mod.vs.log" 2>&1); then
            echo "FAIL  $what: verifyschedule (see $dir/$mod.vs.log)"; failed=1; continue
        fi
        lf=$(sed -n 's/^verifyschedule: wrote //p' "$dir/$mod.vs.log")
        start=$(date +%s)
        if ! (cd "$dir" && "$LEAN" "$lf" > "$mod.lean.log" 2>&1) \
                || grep -q "error\|sorryAx" "$dir/$mod.lean.log"; then
            echo "FAIL  $what: lean (see $dir/$mod.lean.log)"; failed=1; continue
        fi
        took=$(( $(date +%s) - start ))
        # mutants of the hardware must fail
        mutants=""
        if [ "$MUTANTS" -gt 0 ]; then
            if ! (cd "$dir" && python3 "$HERE/mutate.py" -n "$MUTANTS" "$lf") > "$dir/$mod.mutate.log" 2>&1; then
                echo "FAIL  $what: $(tail -1 "$dir/$mod.mutate.log")"; failed=1; continue
            fi
            mutants=", $(sed -n 's| mutants fail to check.*| mutants fail|p' "$dir/$mod.mutate.log")"
        fi
        echo "ok    $what  (${took}s$mutants)"
    done
done

if [ $failed = 0 ] && [ $keep = 0 ]; then rm -rf "$OUT"; else echo "outputs in $OUT"; fi
exit $failed
