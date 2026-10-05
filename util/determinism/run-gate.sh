#!/usr/bin/env bash
# run-gate.sh -- the bsc determinism gate, end to end.
#
# usage: run-gate.sh [inst-dir] [archive-root] [compare.py options...]
#
# Runs the testsuite twice with archive-pass.sh, once normally and once with
# TEST_BSC_OPTIONS=-reverse-intern-order, archiving the generated files under
# <archive-root>/normal and <archive-root>/reversed, then compares the two
# archives with compare.py against the allowlist.txt and ignore.txt committed
# next to this script.  Exits with compare.py's status: 0 green, 1 a file
# differs that is neither allow-listed nor ignored, 2 stale allow-list
# entries, 3 usage or I/O error.
#
# The two passes run at the same time, each with half the jobs, in two
# temporary detached worktrees of this checkout's HEAD under
# <archive-root>/trees/ (a pass writes into its testsuite tree, so two
# passes cannot share one).  bsc records the absolute path of every source
# file in the .bo and .ba it writes, and the testsuite's transcripts quote
# it, so the two trees must be seen at the SAME path or every such file
# would differ: each pass runs in a private mount namespace in which its
# worktree is bind-mounted over this checkout's path (unshare --user
# --mount), and setpriv then drops the namespace's capabilities, so that
# file modes keep their meaning for the tests that make files unreadable
# (a user namespace's root would read them).  A pre-flight probe tries
# exactly that; where it is not available (no unprivileged user
# namespaces), the passes run one after the other in this checkout with all
# the jobs, which SEQUENTIAL=1 asks for explicitly.
#
#   <inst-dir>      the bsc installation to test; defaults to $TEST_RELEASE,
#                   then <worktree>/inst
#   <archive-root>  defaults to $GATE_OUT, then <worktree>/build/determinism;
#                   a previous run's normal/, reversed/, trees/, pass-*.log
#                   and compare.* there are removed first
#   options starting with '--' are passed on to compare.py, e.g.
#   --allow-stale, --root-counts, --write-allowlist F, --jobs N
#
# environment:
#   ONLY_GROUPS="a b c"   restrict both passes to these top-level groups; the
#                         same list goes to compare.py as --scope-groups so
#                         that the other groups' allow-list entries are not
#                         reported stale
#   JOBS=N                make -j for the suite (default: the CPU count);
#                         concurrent passes get half each, sequential passes
#                         all of it
#   SEQUENTIAL=1          run the passes one after the other in this checkout
#   KEEP_TREES=1          keep the temporary worktrees after the run
#   SERIAL=1              passed on to archive-pass.sh: its original one
#                         'make <group>.group' at a time mode
#   GATE_TIMING=FILE      a timing table for archive-pass.sh (slowest tests
#                         first); default <archive-root>/timing.txt, which
#                         every run leaves behind from its normal pass, so the
#                         second run in an archive root is scheduled by the
#                         first one's times
#   TEST_RELEASE, GATE_OUT   defaults for the two positional arguments (CI
#                         sets these and passes no arguments)
set -u -o pipefail

die() { printf 'run-gate.sh: %s\n' "$*" >&2; exit 3; }
usage() { die 'usage: run-gate.sh [inst-dir] [archive-root] [compare.py options...]'; }
say() { echo "== run-gate: $*"; }
load_now() { uptime 2>/dev/null | sed -n 's/.*load average[s]*: */load /p'; }

here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P) || die 'cannot locate myself'
worktree=$(cd "$here/../.." && pwd -P) || die 'cannot locate the worktree'

positional=()
compare_opts=()
want_value=
for arg in "$@"; do
  if [ -n "$want_value" ]; then
    compare_opts+=("$arg"); want_value=
    continue
  fi
  case $arg in
    --write-allowlist|--jobs|--scope-groups) compare_opts+=("$arg"); want_value=$arg ;;
    --*) compare_opts+=("$arg") ;;
    *) positional+=("$arg") ;;
  esac
done
[ -z "$want_value" ] || die "$want_value needs a value"
[ ${#positional[@]} -le 2 ] || usage
inst=${positional[0]:-${TEST_RELEASE:-$worktree/inst}}
root=${positional[1]:-${GATE_OUT:-$worktree/build/determinism}}
allowlist=$here/allowlist.txt
ignore=$here/ignore.txt
[ -f "$allowlist" ] || die "missing $allowlist"
[ -f "$ignore" ] || die "missing $ignore"
[ -x "$inst/bin/bsc" ] || die "no bsc installation at '$inst' (expected $inst/bin/bsc)"
inst=$(cd "$inst" && pwd -P) || die "cannot enter '$inst'"
command -v python3 > /dev/null || die 'python3 not found'
ncpu=$(nproc 2>/dev/null || getconf _NPROCESSORS_ONLN 2>/dev/null || echo 2)
jobs=${JOBS:-$ncpu}
[ "$jobs" -ge 1 ] 2>/dev/null || die "JOBS must be a positive integer, not '$jobs'"
sequential=${SEQUENTIAL:-0}

# Fail now, not two suite runs later, if this bsc does not know the flag.
scratch=$(mktemp -d "${TMPDIR:-/tmp}/run-gate.XXXXXX") || die 'mktemp failed'
printf 'package Empty where\n' > "$scratch/Empty.bs"
if ! "$inst/bin/bsc" -reverse-intern-order -bdir "$scratch" "$scratch/Empty.bs" > "$scratch/bsc.out" 2>&1; then
  cat "$scratch/bsc.out" >&2
  rm -rf "$scratch"
  die "$inst/bin/bsc does not accept -reverse-intern-order"
fi
rm -rf "$scratch"

mkdir -p "$root" || die "cannot create '$root'"
root=$(cd "$root" && pwd -P) || die "cannot enter '$root'"
case $root/ in
  "$worktree"/testsuite/*) die "archive root '$root' is inside the testsuite" ;;
esac

# in_tree_at <tree> <path> <cmd...>: run <cmd> in a private mount namespace
# with <tree> bind-mounted over <path>.  Only the creator of a user namespace
# may mount in it, so unshare maps us to its root; setpriv then empties the
# bounding and inheritable sets, so that nothing exec'd after it holds a
# capability (uid 0 with none behaves as an ordinary owner towards file
# modes).  97 is the mount's own failure.
in_tree_at() {
  local tree=$1 path=$2; shift 2
  unshare --user --map-root-user --mount -- sh -c '
    mount --bind "$1" "$2" || exit 97
    shift 2
    exec setpriv --bounding-set=-all --inh-caps=-all -- "$@"' in_tree_at "$tree" "$path" "$@"
}

# Does in_tree_at work here?  Checks what the passes rely on: the tree shows
# at the path, also through pwd -P (the paths bsc records come from the
# physical cwd), and a mode-000 file is unreadable inside.  Prints the
# reason when not.
probe_sandbox() {
  local d=$root/probe out
  command -v unshare > /dev/null || { echo 'unshare not found'; return 1; }
  command -v setpriv > /dev/null || { echo 'setpriv not found'; return 1; }
  rm -rf "$d"; mkdir -p "$d/tree" || { echo "cannot create $d"; return 1; }
  echo probe > "$d/tree/gate-probe"
  : > "$d/tree/locked"; chmod 000 "$d/tree/locked"
  out=$(in_tree_at "$d/tree" "$worktree" sh -c '
    cd "$1" || { echo "cannot cd to $1"; exit 1; }
    [ "$(pwd -P)" = "$1" ] || { echo "pwd -P is $(pwd -P), not $1"; exit 1; }
    [ -f gate-probe ] || { echo "the bind mount is not visible at $1"; exit 1; }
    if cat locked > /dev/null 2>&1; then echo "a mode-000 file is readable inside the namespace"; exit 1; fi
    echo ok' probe "$worktree" 2>&1)
  chmod 600 "$d/tree/locked"; rm -rf "$d"
  [ "$out" = ok ] || { echo "${out:-no output from the probe}"; return 1; }
}

remove_trees() {
  local t
  for t in "$root"/trees/*/; do
    [ -d "$t" ] || continue
    git -C "$worktree" worktree remove --force "$t" > /dev/null 2>&1 || rm -rf "$t"
  done
  rm -rf "$root/trees"
  git -C "$worktree" worktree prune > /dev/null 2>&1
}

remove_trees
rm -rf "$root/normal" "$root/reversed" "$root"/pass-*.log "$root"/compare.* "$root/archive-pass.sh"
timing=${GATE_TIMING:-$root/timing.txt}
[ -s "$timing" ] || timing=
export GATE_TIMING=$timing

gate_t0=$(date +%s)
say "worktree $worktree  inst $inst  archives $root  $(date '+%F %T')  $(load_now)"
# The allow-list is a baseline for one bsc build (see the header of
# allowlist.txt); say which build this is so a mismatch is visible in the log.
say "$("$inst/bin/bsc" -v 2>&1 | head -n 1)"
say "commit $(git -C "$worktree" rev-parse --short HEAD 2>/dev/null || echo '?')  jobs $jobs$([ "${SERIAL:-0}" = 1 ] && echo '  SERIAL=1 (one group at a time inside each pass)')"
say "timing table: ${timing:-none (first run here: tests start in directory order)}"
if [ -n "${ONLY_GROUPS:-}" ]; then
  say "ONLY_GROUPS='$ONLY_GROUPS'"
  compare_opts+=(--scope-groups "$ONLY_GROUPS")
fi

if [ "$sequential" != 1 ]; then
  if reason=$(probe_sandbox); then
    say "same-path sandbox: ok (unshare --user --mount + setpriv)"
  else
    say "same-path sandbox unavailable: $reason"
    say "running the passes one after the other in $worktree instead"
    sequential=1
  fi
fi

pass_status() {  # pass_status <name> <exit>: report a pass's exit, 0 when fine
  local name=$1 st=$2
  if [ "$st" -eq 97 ]; then
    say "$name: the bind mount failed inside the namespace (see pass-$name.log)"
  elif [ "$st" -ne 0 ]; then
    say "$name: archive-pass.sh exited $st (see pass-$name.log)"
  fi
  return "$st"
}

if [ "$sequential" = 1 ]; then
  say "sequential: two passes in $worktree with $jobs jobs each"
  t0=$(date +%s)
  JOBS=$jobs "$here/archive-pass.sh" "$inst" "$root/normal" 2>&1 | tee "$root/pass-normal.log" \
    || die "archive-pass.sh (normal) failed"
  t1=$(date +%s)
  JOBS=$jobs "$here/archive-pass.sh" "$inst" "$root/reversed" -reverse-intern-order 2>&1 | tee "$root/pass-reversed.log" \
    || die "archive-pass.sh (reversed) failed"
  t2=$(date +%s)
  say "pass wall times: normal $((t1 - t0))s  reversed $((t2 - t1))s"
else
  half=$(( (jobs + 1) / 2 ))
  if [ -n "$(git -C "$worktree" status --porcelain --untracked-files=no -- testsuite 2>/dev/null)" ]; then
    say "WARNING: uncommitted changes under testsuite/ are not in the temporary worktrees (they check out HEAD)"
  fi
  mkdir -p "$root/trees" || die "cannot create $root/trees"
  for name in normal reversed; do
    git -C "$worktree" worktree add --detach "$root/trees/$name" HEAD > "$root/pass-$name.log" 2>&1 \
      || { cat "$root/pass-$name.log"; remove_trees; die "git worktree add $root/trees/$name failed"; }
  done
  # The passes run a copy of archive-pass.sh from outside the mounted path:
  # inside a namespace this checkout's path shows the temporary worktree,
  # whose util/determinism/ is HEAD's, not necessarily this one's.
  cp "$here/archive-pass.sh" "$root/archive-pass.sh" || die "cannot copy archive-pass.sh to $root"
  say "concurrent: two passes in $root/trees/{normal,reversed}, each seen at $worktree, $half jobs each"
  say "pass logs: $root/pass-normal.log  $root/pass-reversed.log"
  cd "$root" || die "cannot enter $root"
  t0=$(date +%s)
  GATE_WORKTREE=$worktree JOBS=$half in_tree_at "$root/trees/normal" "$worktree" \
    bash "$root/archive-pass.sh" "$inst" "$root/normal" >> "$root/pass-normal.log" 2>&1 &
  pid_normal=$!
  GATE_WORKTREE=$worktree JOBS=$half in_tree_at "$root/trees/reversed" "$worktree" \
    bash "$root/archive-pass.sh" "$inst" "$root/reversed" -reverse-intern-order >> "$root/pass-reversed.log" 2>&1 &
  pid_reversed=$!
  wait "$pid_normal"; st_normal=$?; t1=$(date +%s)
  wait "$pid_reversed"; st_reversed=$?; t2=$(date +%s)
  cat "$root/pass-normal.log" "$root/pass-reversed.log"
  say "pass wall times: normal $((t1 - t0))s  reversed $((t2 - t0))s (both started together)"
  [ "${KEEP_TREES:-0}" = 1 ] && say "KEEP_TREES=1: worktrees kept under $root/trees/" || remove_trees
  rm -f "$root/archive-pass.sh"
  pass_status normal "$st_normal" || die "archive-pass.sh (normal) failed"
  pass_status reversed "$st_reversed" || die "archive-pass.sh (reversed) failed"
fi

# keep the normal pass's timing table for the next run in this archive root
[ -s "$root/normal/_log/timing.txt" ] && cp "$root/normal/_log/timing.txt" "$root/timing.txt"

say "comparing  $(date '+%F %T')"
python3 "$here/compare.py" "$root/normal" "$root/reversed" \
  --allowlist "$allowlist" --ignore "$ignore" --out "$root/compare" \
  ${compare_opts[@]+"${compare_opts[@]}"}
status=$?
say "done, exit $status  $(( $(date +%s) - gate_t0 ))s wall  $(load_now)  $(date '+%F %T')"
exit "$status"
