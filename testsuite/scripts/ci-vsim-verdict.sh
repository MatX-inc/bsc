#!/usr/bin/env bash
# ci-vsim-verdict.sh — collect, seed, and judge testsuite results for the
# CI verilator leg, against committed manifests.
#
#   collect <testsuite-dir>
#       Emit one classified line per noteworthy testrun.sum entry:
#           <sumdir> :: <RESULT>: <test text>
#       for RESULT in the fail class (FAIL XPASS KPASS UNRESOLVED ERROR)
#       and for UNSUPPORTED (the skip-layer contract).  The full-suite
#       job redirects this to observed.txt.
#
#   seed <testsuite-dir> <manifest-dir>
#       Write fails-<platform>.txt and unsupported-<platform>.txt from a
#       (local or CI) run's testrun.sum tree.  Entries should then be
#       hand-annotated with '#'-comment dispositions before committing.
#
#   judge <mode> <manifest-dir> <results-dir> [testsuite-dir]
#       Grade a full run's observed.txt, schedule.mk, and suite-rc.txt
#       against the manifests.  mode: observe | ratchet | gate
#       (anything else: red).  Missing/incomplete run artifacts and
#       scheduled-vs-in-tree coverage holes are red in every mode.
#       Additional red conditions by mode:
#         observe: none (content ledger only)
#         ratchet: NEW fail-class entries not in the manifest, or any
#                  UNSUPPORTED-set drift
#         gate:    as ratchet, plus stale manifest entries, any observed
#                  fail-class entry at all, or a non-empty fails manifest
#       The summary table and delta lines are ALWAYS published (to
#       $GITHUB_STEP_SUMMARY when set, stdout otherwise) — a red or
#       green verdict is never silent about what it saw.
#
# All sorting under LC_ALL=C for cross-machine stability.

set -euo pipefail
export LC_ALL=C

PLATFORM="${VSIM_PLATFORM:-linux-x86_64}"
FAIL_RE='^(FAIL|XPASS|KPASS|UNRESOLVED|ERROR): '
UNSUP_RE='^UNSUPPORTED: '
workdir=$(mktemp -d "${TMPDIR:-/tmp}/ci-vsim-verdict.XXXXXX")
trap 'rm -rf -- "$workdir"' EXIT

say() {
    if [ -n "${GITHUB_STEP_SUMMARY:-}" ]; then
        echo "$*" >> "$GITHUB_STEP_SUMMARY"
    fi
    echo "$*"
}

collect_tree() {
    local root="$1"
    ( cd "$root" && find . -name testrun.sum | sort ) | while read -r sum; do
        local dir
        dir=$(dirname "$sum" | sed 's|^\./||')
        grep -E "$FAIL_RE|$UNSUP_RE" "$root/$sum" 2>/dev/null | \
            sed "s|^|$dir :: |" || true
    done
}

case "${1:-}" in

collect)
    root="${2:?usage: $0 collect <testsuite-dir>}"
    collect_tree "$root"
    ;;

seed)
    root="${2:?usage: $0 seed <testsuite-dir> <manifest-dir>}"
    mdir="${3:?usage: $0 seed <testsuite-dir> <manifest-dir>}"
    mkdir -p "$mdir"
    collect_tree "$root" > "$workdir/seed"
    grep -E " :: (FAIL|XPASS|KPASS|UNRESOLVED|ERROR): " "$workdir/seed" \
        | sort -u > "$mdir/fails-$PLATFORM.txt" || true
    grep -E " :: UNSUPPORTED: " "$workdir/seed" \
        | sort -u > "$mdir/unsupported-$PLATFORM.txt" || true
    touch "$mdir/flaky-$PLATFORM.txt"
    echo "seeded: $(wc -l < "$mdir/fails-$PLATFORM.txt") fail entries," \
         "$(wc -l < "$mdir/unsupported-$PLATFORM.txt") unsupported entries"
    echo "annotate entries with whole-line '#' comments before committing (trailing comments are not supported: entries are exact line keys and reasons may contain '#')"
    ;;

judge)
    mode="${2:?usage: $0 judge <mode> <manifest-dir> <results-dir> [testsuite-dir]}"
    mdir="${3:?}"
    fdir="${4:?}"
    tsdir="${5:-testsuite}"

    case "$mode" in
        observe|ratchet|gate) ;;
        *) say "VERDICT: RED — unknown mode '$mode' (want observe|ratchet|gate)"; exit 1 ;;
    esac

    red=0
    reasons=()

    # --- completeness: the full run returned and published its artifacts
    missing=0
    for f in observed.txt schedule.mk suite-rc.txt; do
        if [ ! -f "$fdir/$f" ]; then
            say "MISSING result: $f"
            missing=1
        fi
    done
    if [ "$missing" -eq 1 ]; then
        reasons+=("incomplete: missing full-suite result(s) — the run died or never completed")
        red=1
    fi

    suite_rc=missing
    if [ -f "$fdir/suite-rc.txt" ]; then
        suite_rc=$(head -1 "$fdir/suite-rc.txt" | tr -d '[:space:]')
        case "$suite_rc" in
            0|2) ;;
            *) reasons+=("infrastructure: testsuite command returned rc=$suite_rc")
               red=1 ;;
        esac
    fi

    # --- coverage: scheduled tests vs in-tree .exp files
    if [ -f "$fdir/schedule.mk" ] && [ -d "$tsdir" ]; then
        cat "$fdir/schedule.mk" \
            | tr ' ' '\n' | grep '\.exp$' | sed 's|^\./||' | sort -u > "$workdir/scheduled"
        ( cd "$tsdir" && find bsc.* -name '*.exp' 2>/dev/null ) \
            | grep -v '^bsc\.long_tests/' | sed 's|^\./||' | sort -u > "$workdir/intree"
        comm -13 "$workdir/scheduled" "$workdir/intree" > "$workdir/holes"
        if [ -s "$workdir/holes" ]; then
            say "COVERAGE HOLES (in-tree tests not scheduled):"
            head -50 "$workdir/holes" | sed 's/^/    /' | while read -r l; do say "$l"; done
            reasons+=("coverage: $(wc -l < "$workdir/holes") in-tree .exp never scheduled")
            red=1
        fi
    else
        say "note: schedule or testsuite dir absent; coverage check skipped"
        reasons+=("coverage: schedule or testsuite tree absent — cannot prove nothing was dropped")
        red=1
    fi

    # --- execution: every scheduled directory must have produced results.
    # collect only emits noteworthy lines, so an all-PASS directory and a
    # directory whose runtest never ran (crashed, or killed by the job
    # timeout) look identical in observed.txt; the testrun.sum files are
    # the only witness that a directory actually executed.  A missing one
    # is infrastructure-red in every mode -- silence must never grade green.
    if [ -f "$fdir/schedule.mk" ] && [ -d "$tsdir" ]; then
        cat "$fdir/schedule.mk" \
            | tr ' ' '\n' | grep '\.exp$' | sed 's|^\./||' | xargs -r -n1 dirname \
            | sort -u > "$workdir/sched-dirs"
        : > "$workdir/missing-sums"
        while read -r d; do
            [ -f "$tsdir/$d/testrun.sum" ] || echo "$d" >> "$workdir/missing-sums"
        done < "$workdir/sched-dirs"
        if [ -s "$workdir/missing-sums" ]; then
            say "EXECUTION HOLES (scheduled dirs with no testrun.sum):"
            head -50 "$workdir/missing-sums" | sed 's/^/    /' | while read -r l; do say "$l"; done
            reasons+=("execution: $(wc -l < "$workdir/missing-sums") scheduled dir(s) produced no testrun.sum")
            red=1
        fi
    fi

    # --- observed sets
    if [ -f "$fdir/observed.txt" ]; then
        sort -u "$fdir/observed.txt" > "$workdir/observed"
    else
        touch "$workdir/observed"
    fi
    grep -E " :: (FAIL|XPASS|KPASS|UNRESOLVED|ERROR): " "$workdir/observed" > "$workdir/obs-fails-raw" || true
    grep -E " :: UNSUPPORTED: " "$workdir/observed" > "$workdir/obs-unsup" || true

    # manifests (drop whole-line comments and blanks; entries are exact
    # line keys, and reason strings may legitimately contain '#', so
    # trailing comments are NOT supported -- annotate on their own line)
    for f in fails unsupported flaky; do
        sed -e '/^[[:space:]]*#/d' -e '/^[[:space:]]*$/d' \
            "$mdir/$f-$PLATFORM.txt" 2>/dev/null | sort -u > "$workdir/man-$f" || true
        touch "$workdir/man-$f"
    done

    # flaky entries are excused from the observed fail set (but counted)
    grep -Fvxf "$workdir/man-flaky" "$workdir/obs-fails-raw" > "$workdir/obs-fails" || true
    flaky_hits=$(grep -Fxf "$workdir/man-flaky" "$workdir/obs-fails-raw" | wc -l || true)

    comm -13 "$workdir/man-fails" "$workdir/obs-fails" > "$workdir/new-fails" || true
    comm -23 "$workdir/man-fails" "$workdir/obs-fails" > "$workdir/stale-fails" || true

    unsup_drift=0
    if ! diff -u "$workdir/man-unsupported" "$workdir/obs-unsup" > "$workdir/unsup-diff" 2>&1; then
        unsup_drift=1
    fi

    # --- publish the ledger, always
    say "### MatX Verilator verdict (mode: $mode, platform: $PLATFORM)"
    say ""
    say "Testsuite command rc: $suite_rc"
    say ""
    say "| set | count |"
    say "|---|---|"
    say "| observed fail-class (raw) | $(wc -l < "$workdir/obs-fails-raw") |"
    say "| excused as flaky | $flaky_hits |"
    say "| manifest fails | $(wc -l < "$workdir/man-fails") |"
    say "| NEW fails (not in manifest) | $(wc -l < "$workdir/new-fails") |"
    say "| stale manifest entries (now passing) | $(wc -l < "$workdir/stale-fails") |"
    say "| observed UNSUPPORTED | $(wc -l < "$workdir/obs-unsup") |"
    say "| manifest UNSUPPORTED | $(wc -l < "$workdir/man-unsupported") |"
    say "| UNSUPPORTED drift | $unsup_drift |"
    say ""
    if [ -s "$workdir/new-fails" ]; then
        say "NEW failures (first 100):"
        head -100 "$workdir/new-fails" | while read -r l; do say "    $l"; done
    fi
    if [ -s "$workdir/stale-fails" ]; then
        say "Stale manifest entries (first 100):"
        head -100 "$workdir/stale-fails" | while read -r l; do say "    $l"; done
    fi
    if [ "$unsup_drift" -eq 1 ]; then
        say "UNSUPPORTED drift (first 100 diff lines):"
        head -100 "$workdir/unsup-diff" | while read -r l; do say "    $l"; done
    fi

    # --- exit by mode
    case "$mode" in
        observe)
            say "VERDICT: observe mode — content ledger recorded without content gating"
            ;;
        ratchet)
            if [ -s "$workdir/new-fails" ]; then
                reasons+=("$(wc -l < "$workdir/new-fails") NEW failure(s) vs manifest")
                red=1
            fi
            if [ "$unsup_drift" -eq 1 ]; then
                reasons+=("UNSUPPORTED set drifted from the committed skip contract")
                red=1
            fi
            if [ -s "$workdir/stale-fails" ]; then
                say "::warning::$(wc -l < "$workdir/stale-fails") stale manifest entries now pass; shrink the manifest"
            fi
            ;;
        gate)
            if [ -s "$workdir/obs-fails" ]; then
                reasons+=("gate mode: $(wc -l < "$workdir/obs-fails") fail-class result(s) observed")
                red=1
            fi
            if [ -s "$workdir/man-fails" ]; then
                reasons+=("gate mode: fails manifest is non-empty — burn it down before gating")
                red=1
            fi
            if [ -s "$workdir/stale-fails" ]; then
                reasons+=("gate mode: stale manifest entries")
                red=1
            fi
            if [ "$unsup_drift" -eq 1 ]; then
                reasons+=("UNSUPPORTED set drifted from the committed skip contract")
                red=1
            fi
            ;;
    esac

    if [ "$red" -eq 1 ]; then
        say ""
        say "VERDICT: RED"
        for r in "${reasons[@]}"; do say "  - $r"; done
        exit 1
    fi
    say ""
    say "VERDICT: GREEN"
    ;;

*)
    echo "usage: $0 {collect <testsuite-dir> | seed <testsuite-dir> <manifest-dir> | judge <mode> <manifest-dir> <results-dir> [testsuite-dir]}" >&2
    exit 2
    ;;
esac
