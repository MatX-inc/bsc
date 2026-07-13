#!/usr/bin/env bash
# ci-vsim-verdict.sh — collect, seed, and judge testsuite results for the
# CI verilator leg, against committed manifests.
#
#   collect <testsuite-dir>
#       Emit one classified line per noteworthy testrun.sum entry:
#           <sumdir> :: <RESULT>: <test text>
#       for RESULT in the fail class (FAIL XPASS KPASS UNRESOLVED ERROR)
#       and for UNSUPPORTED (the skip-layer contract).  Shard jobs
#       redirect this to observed-shard-N.txt.
#
#   seed <testsuite-dir> <manifest-dir>
#       Write fails-<platform>.txt and unsupported-<platform>.txt from a
#       (local or CI) run's testrun.sum tree.  Entries should then be
#       hand-annotated with '#'-comment dispositions before committing.
#
#   judge <mode> <manifest-dir> <fragments-dir> [testsuite-dir]
#       Grade downloaded observed-shard-*.txt fragments against the
#       manifests.  mode: observe | ratchet | gate (anything else: red).
#       Red conditions by mode:
#         observe: never (ledger only)
#         ratchet: missing/incomplete fragments, scheduled-vs-in-tree
#                  coverage hole, NEW fail-class entries not in the
#                  manifest, or any UNSUPPORTED-set drift
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
SHARDS="${VSIM_SHARDS:-7}"

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
    collect_tree "$root" > /tmp/ci-vsim-seed.$$
    grep -E " :: (FAIL|XPASS|KPASS|UNRESOLVED|ERROR): " /tmp/ci-vsim-seed.$$ \
        | sort > "$mdir/fails-$PLATFORM.txt" || true
    grep -E " :: UNSUPPORTED: " /tmp/ci-vsim-seed.$$ \
        | sort > "$mdir/unsupported-$PLATFORM.txt" || true
    rm -f /tmp/ci-vsim-seed.$$
    touch "$mdir/flaky-$PLATFORM.txt"
    echo "seeded: $(wc -l < "$mdir/fails-$PLATFORM.txt") fail entries," \
         "$(wc -l < "$mdir/unsupported-$PLATFORM.txt") unsupported entries"
    echo "annotate fails-$PLATFORM.txt entries with '#' disposition comments before committing"
    ;;

judge)
    mode="${2:?usage: $0 judge <mode> <manifest-dir> <fragments-dir> [testsuite-dir]}"
    mdir="${3:?}"
    fdir="${4:?}"
    tsdir="${5:-testsuite}"

    case "$mode" in
        observe|ratchet|gate) ;;
        *) say "VERDICT: RED — unknown mode '$mode' (want observe|ratchet|gate)"; exit 1 ;;
    esac

    red=0
    reasons=()

    # --- completeness: every shard fragment present and non-trivial
    missing=0
    for n in $(seq 1 "$SHARDS"); do
        if [ ! -f "$fdir/observed-shard-$n.txt" ]; then
            say "MISSING fragment: observed-shard-$n.txt"
            missing=1
        fi
    done
    if [ "$missing" -eq 1 ]; then
        reasons+=("incomplete: missing shard fragment(s) — a shard died or never ran")
        red=1
    fi

    # --- coverage: union of scheduled tests vs in-tree .exp files
    if ls "$fdir"/schedule-shard-*.mk >/dev/null 2>&1 && [ -d "$tsdir" ]; then
        cat "$fdir"/schedule-shard-*.mk \
            | tr ' ' '\n' | grep '\.exp$' | sed 's|^\./||' | sort -u > /tmp/scheduled.$$
        ( cd "$tsdir" && find bsc.* -name '*.exp' 2>/dev/null ) \
            | grep -v '^bsc\.long_tests/' | sed 's|^\./||' | sort -u > /tmp/intree.$$
        comm -13 /tmp/scheduled.$$ /tmp/intree.$$ > /tmp/holes.$$
        if [ -s /tmp/holes.$$ ]; then
            say "COVERAGE HOLES (in-tree tests no shard scheduled):"
            head -50 /tmp/holes.$$ | sed 's/^/    /' | while read -r l; do say "$l"; done
            reasons+=("coverage: $(wc -l < /tmp/holes.$$) in-tree .exp never scheduled by any shard")
            red=1
        fi
        rm -f /tmp/scheduled.$$ /tmp/intree.$$ /tmp/holes.$$
    else
        say "note: schedule fragments or testsuite dir absent; coverage check skipped"
        if [ "$mode" != "observe" ]; then
            reasons+=("coverage: schedule fragments absent — cannot prove nothing was dropped")
            red=1
        fi
    fi

    # --- observed sets
    cat "$fdir"/observed-shard-*.txt 2>/dev/null | sort -u > /tmp/observed.$$
    grep -E " :: (FAIL|XPASS|KPASS|UNRESOLVED|ERROR): " /tmp/observed.$$ > /tmp/obs-fails-raw.$$ || true
    grep -E " :: UNSUPPORTED: " /tmp/observed.$$ > /tmp/obs-unsup.$$ || true

    # manifests (strip comments/blank lines)
    for f in fails unsupported flaky; do
        sed -e 's/[[:space:]]*#.*$//' -e '/^[[:space:]]*$/d' \
            "$mdir/$f-$PLATFORM.txt" 2>/dev/null | sort -u > "/tmp/man-$f.$$" || true
        touch "/tmp/man-$f.$$"
    done

    # flaky entries are excused from the observed fail set (but counted)
    grep -Fvxf /tmp/man-flaky.$$ /tmp/obs-fails-raw.$$ > /tmp/obs-fails.$$ || true
    flaky_hits=$(grep -Fxf /tmp/man-flaky.$$ /tmp/obs-fails-raw.$$ | wc -l || true)

    comm -13 /tmp/man-fails.$$ /tmp/obs-fails.$$ > /tmp/new-fails.$$ || true
    comm -23 /tmp/man-fails.$$ /tmp/obs-fails.$$ > /tmp/stale-fails.$$ || true

    unsup_drift=0
    if ! diff -u /tmp/man-unsupported.$$ /tmp/obs-unsup.$$ > /tmp/unsup-diff.$$ 2>&1; then
        unsup_drift=1
    fi

    # --- publish the ledger, always
    say "### MatX Verilator verdict (mode: $mode, platform: $PLATFORM)"
    say ""
    say "| set | count |"
    say "|---|---|"
    say "| observed fail-class (raw) | $(wc -l < /tmp/obs-fails-raw.$$) |"
    say "| excused as flaky | $flaky_hits |"
    say "| manifest fails | $(wc -l < /tmp/man-fails.$$) |"
    say "| NEW fails (not in manifest) | $(wc -l < /tmp/new-fails.$$) |"
    say "| stale manifest entries (now passing) | $(wc -l < /tmp/stale-fails.$$) |"
    say "| observed UNSUPPORTED | $(wc -l < /tmp/obs-unsup.$$) |"
    say "| manifest UNSUPPORTED | $(wc -l < /tmp/man-unsupported.$$) |"
    say "| UNSUPPORTED drift | $unsup_drift |"
    say ""
    if [ -s /tmp/new-fails.$$ ]; then
        say "NEW failures (first 100):"
        head -100 /tmp/new-fails.$$ | while read -r l; do say "    $l"; done
    fi
    if [ -s /tmp/stale-fails.$$ ]; then
        say "Stale manifest entries (first 100):"
        head -100 /tmp/stale-fails.$$ | while read -r l; do say "    $l"; done
    fi
    if [ "$unsup_drift" -eq 1 ]; then
        say "UNSUPPORTED drift (first 100 diff lines):"
        head -100 /tmp/unsup-diff.$$ | while read -r l; do say "    $l"; done
    fi

    # --- exit by mode
    case "$mode" in
        observe)
            say "VERDICT: observe mode — ledger recorded, always green"
            ;;
        ratchet)
            if [ -s /tmp/new-fails.$$ ]; then
                reasons+=("$(wc -l < /tmp/new-fails.$$) NEW failure(s) vs manifest")
                red=1
            fi
            if [ "$unsup_drift" -eq 1 ]; then
                reasons+=("UNSUPPORTED set drifted from the committed skip contract")
                red=1
            fi
            if [ -s /tmp/stale-fails.$$ ]; then
                say "::warning::$(wc -l < /tmp/stale-fails.$$) stale manifest entries now pass; shrink the manifest"
            fi
            ;;
        gate)
            if [ -s /tmp/obs-fails.$$ ]; then
                reasons+=("gate mode: $(wc -l < /tmp/obs-fails.$$) fail-class result(s) observed")
                red=1
            fi
            if [ -s /tmp/man-fails.$$ ]; then
                reasons+=("gate mode: fails manifest is non-empty — burn it down before gating")
                red=1
            fi
            if [ -s /tmp/stale-fails.$$ ]; then
                reasons+=("gate mode: stale manifest entries")
                red=1
            fi
            if [ "$unsup_drift" -eq 1 ]; then
                reasons+=("UNSUPPORTED set drifted from the committed skip contract")
                red=1
            fi
            ;;
    esac

    rm -f /tmp/observed.$$ /tmp/obs-fails-raw.$$ /tmp/obs-fails.$$ /tmp/obs-unsup.$$ \
          /tmp/man-fails.$$ /tmp/man-unsupported.$$ /tmp/man-flaky.$$ \
          /tmp/new-fails.$$ /tmp/stale-fails.$$ /tmp/unsup-diff.$$

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
    echo "usage: $0 {collect <testsuite-dir> | seed <testsuite-dir> <manifest-dir> | judge <mode> <manifest-dir> <fragments-dir> [testsuite-dir]}" >&2
    exit 2
    ;;
esac
