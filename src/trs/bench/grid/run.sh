#!/bin/sh
# Grid scaling benchmark: for each N, generate Grid<N>.bsv (N*N
# replicated always-fire tiles), build the reference Bluesim
# executable and the trs AOT artifact from the same .bir, run both,
# diff stdout byte-for-byte, and append one CSV row of build/run/RSS
# numbers per N to results.csv.
#
#   BSC=/path/to/bsc TRS=/path/to/trs [NS="2 4 8"] [CYCLES=1000] \
#       sh run.sh [workdir]
#
# results.csv lands in the invoking directory (override with RESULTS=);
# per-N build logs and outputs stay in <workdir>/N<n>/.
BSC=${BSC:-bsc}
TRS=${TRS:-trs}
NS=${NS:-"2 4 8"}
TILE=${TILE:-v3}
CYCLES=${CYCLES:-1000}
SRC=$(cd "$(dirname "$0")" && pwd)
RESULTS=${RESULTS:-$PWD/results.csv}
# the reference executable is a script that needs bluetcl on PATH
case "$BSC" in
    */*) PATH="$(cd "$(dirname "$BSC")" && pwd):$PATH"; export PATH;;
esac
# the trs artifact wrapper honors $TRS (else wants trs on PATH)
case "$TRS" in
    */*) TRS="$(cd "$(dirname "$TRS")" && pwd)/$(basename "$TRS")"
         PATH="$(dirname "$TRS"):$PATH"; export PATH;;
esac
export TRS
WK=${1:-$(mktemp -d)}
mkdir -p "$WK" || exit 2
WK=$(cd "$WK" && pwd)

[ -f "$RESULTS" ] || echo "gen,N,tiles,bsc_frontend_s,ref_build_s,b3_link_s,ref_run_s,b3_run_s,ref_rss_kb,b3_rss_kb,ir_passes_s,backend_s" > "$RESULTS"

now() { date +%s.%N; }
dur() { awk -v a="$1" -v b="$2" 'BEGIN { printf "%.3f", b - a }'; }
rss() { awk -F: '/Maximum resident set size/ { gsub(/[^0-9]/, "", $2); print $2 }' "$1"; }
# last TRS_JIT_TIME phase line containing $2, Duration rendered as
# seconds ("1.23s" / "45.6ms" / "789µs" / "12ns" -> 1.230 / 0.046 / ...)
phase() {
    awk -v p="$2" '
        index($0, p) { v = $NF }
        END {
            if (v == "") exit
            sub(/s$/, "", v)
            f = 1
            if      (v ~ /m$/) { f = 1e3; sub(/m$/, "", v) }
            else if (v ~ /µ$/) { f = 1e6; sub(/µ$/, "", v) }
            else if (v ~ /u$/) { f = 1e6; sub(/u$/, "", v) }
            else if (v ~ /n$/) { f = 1e9; sub(/n$/, "", v) }
            printf "%.3f", v / f
        }' "$1"
}

fail=0
bench() { # n
    n=$1
    m=$((n * n))
    top=sysGrid$n
    d="$WK/N$n"
    rm -rf "$d"
    mkdir -p "$d"
    cd "$d" || exit 2

    python3 "$SRC/gen_grid.py" "$n" --tile "$TILE" --cycles "$CYCLES" -o "Grid$n.bsv" \
        || { echo "FAIL N=$n (gen_grid.py)"; fail=1; return; }

    # bsc frontend: parse/typecheck/elaborate to .bo/.ba
    t0=$(now)
    $BSC -sim -bir -u -g "$top" "Grid$n.bsv" > bsc.log 2>&1 \
        || { echo "FAIL N=$n (bsc compile, see $d/bsc.log)"; fail=1; return; }
    t1=$(now); fe_s=$(dur "$t0" "$t1")

    # reference Bluesim build (also exports $top.bir)
    t0=$(now)
    $BSC -sim -bir -e "$top" -o sim.exe >> bsc.log 2>&1 \
        || { echo "FAIL N=$n (bsc link, see $d/bsc.log)"; fail=1; return; }
    t1=$(now); rb_s=$(dur "$t0" "$t1")
    [ -f "$top.bir" ] \
        || { echo "FAIL N=$n (no $top.bir exported)"; fail=1; return; }

    # trs AOT link; TRS_JIT_TIME phase lines land in b3link.log
    t0=$(now)
    TRS_JIT_TIME=1 "$TRS" link "$top.bir" -o b3sim > b3link.log 2>&1 \
        || { echo "FAIL N=$n (trs link, see $d/b3link.log)"; fail=1; return; }
    t1=$(now); lk_s=$(dur "$t0" "$t1")
    grep -q "compiled mode unavailable" b3link.log \
        && echo "WARN N=$n: artifact is interpreted, not compiled (see $d/b3link.log)"
    # phase lines: "trs aot: ir passes 38.4ms", "trs aot: backend
    # emit 25.9ms" (older builds only print "one-module compile")
    ir_s=$(phase b3link.log "ir passes")
    be_s=$(phase b3link.log "backend emit")
    [ -n "$be_s" ] || be_s=$(phase b3link.log "one-module compile")

    # run both (the design $finishes itself at cycle $CYCLES; -m is a
    # safety bound only), /usr/bin/time -v for peak RSS
    mx=$((CYCLES + 100))
    t0=$(now)
    /usr/bin/time -v -o ref.time ./sim.exe -m "$mx" > ref.out 2> ref.err
    rc_ref=$?
    t1=$(now); rr_s=$(dur "$t0" "$t1")
    t0=$(now)
    /usr/bin/time -v -o b3.time ./b3sim -m "$mx" > b3.out 2> b3.err
    rc_b3=$?
    t1=$(now); br_s=$(dur "$t0" "$t1")

    ok=1
    [ "$rc_ref" = 0 ] || { echo "FAIL N=$n (reference exit $rc_ref)"; ok=0; }
    [ "$rc_b3" = 0 ]  || { echo "FAIL N=$n (trs exit $rc_b3)"; ok=0; }
    diff ref.out b3.out > /dev/null \
        || { echo "FAIL N=$n (stdout differs: diff $d/ref.out $d/b3.out)"; ok=0; }
    [ "$ok" = 1 ] || fail=1

    ref_rss=$(rss ref.time)
    b3_rss=$(rss b3.time)
    row="$TILE,$n,$m,$fe_s,$rb_s,$lk_s,$rr_s,$br_s,$ref_rss,$b3_rss,$ir_s,$be_s"
    echo "$row" >> "$RESULTS"
    [ "$ok" = 1 ] && echo "PASS N=$n  $row"
}

for n in $NS; do
    bench "$n"
done
echo "results: $RESULTS  (work: $WK)"
exit $fail
