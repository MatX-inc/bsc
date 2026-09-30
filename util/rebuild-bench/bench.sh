#!/usr/bin/env bash
# P0 rebuild benchmark harness (doc/engine-first-plan.md, phase P0).
# See README.md in this directory. Safe by construction: edits are marker
# comment lines appended to tracked files and are always removed again;
# the script refuses to run on a dirty tree so restoration is checkable.

set -u

TOP="$(cd "$(dirname "$0")/../.." && pwd)"
cd "$TOP"

STAMP="$(date -u +%Y%m%dT%H%M%SZ)"
OUT="$TOP/bench-results/$STAMP"
LOG="$OUT/log"
mkdir -p "$LOG"
RESULTS="$OUT/results.tsv"

MARKER="-- bench-edit marker (rebuild-bench; removed automatically)"
MARKER_SLASH="// bench-edit marker (rebuild-bench; removed automatically)"

ALL_SCENARIOS="cold null clean-outputs leaf-edit frontend-edit evaluator-edit scheduler-edit verilog-edit bluesim-edit library-edit"
SCENARIOS="${*:-$ALL_SCENARIOS}"

edit_target() {
    case "$1" in
        leaf-edit)      echo "src/comp/app/vcdcheck.hs" ;;
        frontend-edit)  echo "src/comp/TCheck.hs" ;;
        evaluator-edit) echo "src/comp/IExpand.hs" ;;
        scheduler-edit) echo "src/comp/ASchedule.hs" ;;
        verilog-edit)   echo "src/comp/AVerilog.hs" ;;
        bluesim-edit)   echo "src/comp/SimCCBlock.hs" ;;
        library-edit)   echo "src/Libraries/Base1/ListN.bs" ;;
        *)              echo "" ;;
    esac
}

marker_for() {
    case "$1" in
        *.bs) echo "$MARKER" ;;   # classic syntax: '--' comments
        *.hs) echo "$MARKER" ;;
        *)    echo "$MARKER_SLASH" ;;
    esac
}

die() { echo "bench.sh: $*" >&2; exit 1; }

command -v /usr/bin/time >/dev/null || die "needs /usr/bin/time (GNU time)"
git -C "$TOP" diff --quiet || die "working tree is dirty; commit or stash first"

# ---------------------------------------------------------------- inventory
{
    echo "date_utc: $(date -u +%FT%TZ)"
    echo "git_rev: $(git rev-parse HEAD)"
    echo "git_status_clean: yes"
    echo "uname: $(uname -a)"
    echo "nproc: $(nproc 2>/dev/null || sysctl -n hw.ncpu)"
    echo "ghc: $(ghc --version 2>&1)"
    echo "cabal: $(cabal --version 2>&1 | head -1)"
    echo "cc: $(${CC:-cc} --version 2>&1 | head -1)"
    echo "BSC_OPTIONS: ${BSC_OPTIONS:-<unset>}"
    echo "PREFIX: ${PREFIX:-<default: \$TOP/inst>}"
    echo "make_oracle_compiler: (not run here) src/comp/Makefile via make"
    echo "make_oracle_libraries: make -C src/Libraries build"
    echo "cabal_project:"
    sed 's/^/  /' cabal.project
} > "$OUT/inventory.txt"

echo -e "scenario\tstage\twall_s\tcpu_s\tmax_rss_kb\tghc_mods\tbsc_pkgs\texit" > "$RESULTS"

# ------------------------------------------------------------------ helpers
run_stage() {  # scenario stage cmd...
    local scenario="$1" stage="$2"; shift 2
    local log="$LOG/$scenario.$stage.log"
    local tfile="$LOG/$scenario.$stage.time"
    local t0 t1 wall cpu rss ghc bscp code
    t0=$(date +%s.%N)
    /usr/bin/time -v -o "$tfile" "$@" >"$log" 2>&1
    code=$?
    t1=$(date +%s.%N)
    wall=$(echo "$t1 $t0" | awk '{printf "%.2f", $1-$2}')
    cpu=$(awk -F': ' '/User time/{u=$2} /System time/{s=$2} END{printf "%.2f", u+s}' "$tfile")
    rss=$(awk -F': ' '/Maximum resident/{print $2}' "$tfile")
    ghc=$(grep -cE '^\[ *[0-9]+ of [0-9]+\] Compiling' "$log" || true)
    bscp=$(grep -cE '^(checking package dependencies|compiling |code generation |Compilation of|All packages are up to date)' "$log" || true)
    echo -e "$scenario\t$stage\t$wall\t$cpu\t${rss:-0}\t$ghc\t$bscp\t$code" >> "$RESULTS"
    echo "  $stage: ${wall}s wall, ${cpu}s cpu, exit $code"
    return $code
}

apply_edit() {  # file
    local f="$1"
    echo "$(marker_for "$f")" >> "$f"
}

revert_edits() {
    git -C "$TOP" checkout -- $(git -C "$TOP" diff --name-only) 2>/dev/null || true
    git -C "$TOP" diff --quiet || die "failed to restore edited files"
}
trap revert_edits EXIT

stages() {  # scenario
    local sc="$1"
    run_stage "$sc" compiler   cabal build
    run_stage "$sc" bsc-only   cabal build bsc:exe:bsc
    run_stage "$sc" libraries  make -C src/Libraries build
    run_stage "$sc" install    make -C src/Libraries install
}

# ---------------------------------------------------------------- scenarios
for sc in $SCENARIOS; do
    echo "== scenario: $sc =="
    case "$sc" in
        cold)
            cabal clean >/dev/null 2>&1
            make -C src/Libraries clean >"$LOG/$sc.prep.log" 2>&1
            rm -rf build/bsvlib
            stages "$sc"
            ;;
        null)
            stages "$sc"
            ;;
        clean-outputs)
            rm -rf build/bsvlib
            stages "$sc"
            ;;
        *-edit)
            f="$(edit_target "$sc")"
            [ -n "$f" ] || die "unknown scenario: $sc"
            apply_edit "$f"
            stages "$sc"
            git -C "$TOP" checkout -- "$f"
            ;;
        *)
            die "unknown scenario: $sc"
            ;;
    esac
done

echo
echo "results: $RESULTS"
column -t -s $'\t' "$RESULTS"
