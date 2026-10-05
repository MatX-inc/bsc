#!/usr/bin/env bash
# run-gate.sh -- the bsc determinism gate, end to end.
#
# usage: run-gate.sh [inst-dir] [archive-root] [compare.py options...]
#
# Runs the testsuite twice with archive-pass.sh, once normally and once with
# -reverse-intern-order in TEST_BSC_OPTIONS, archiving the generated files
# under <archive-root>/normal and <archive-root>/reversed, then compares the
# two archives with compare.py against the allowlist.txt and ignore.txt
# committed next to this script.  Exits with compare.py's status: 0 green,
# 1 a file differs that is neither allow-listed nor ignored, 2 stale
# allow-list entries, 3 usage or I/O error, or a pass that did not complete
# (compare.py still runs, for the record, but its verdict does not count).
#
# The two passes run at the same time, each with half the jobs, in two
# temporary detached worktrees of this checkout's HEAD,
# <archive-root>/trees/normal and <archive-root>/trees/reversed (a pass
# writes into its testsuite tree, so two passes cannot share one).  They
# are plain worktrees at their own paths: nothing is mounted, the checkout
# may be a main repository or a linked worktree, and the installation and
# the archive root may be anywhere outside the trees.  bsc stores the
# absolute path of every source file in the .bo and .ba it writes, which
# would make every such file differ between two trees, so each pass also
# gets -remap-path-prefix <its tree>=. in TEST_BSC_OPTIONS and the stored
# paths read ./testsuite/... in both archives.  (Upstream #1135 takes the
# absolute paths out of the .bo/.ba; once this line carries it the flag is
# a no-op and can go.)  Transcripts that quote a path of their own tree,
# make's 'Entering directory' lines for one, are compare.py's business.
#
#   <inst-dir>      the bsc installation to test; defaults to $TEST_RELEASE,
#                   then <checkout>/inst
#   <archive-root>  defaults to $GATE_OUT, then <checkout>/build/determinism
#                   (git-ignored); a previous run's normal/, reversed/,
#                   trees/, pass-*.log and compare.* there are removed
#                   first.  Not the checkout itself, nor under its testsuite/
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
#   SEQUENTIAL=1          run the passes one after the other, in one
#                         temporary worktree (trees/normal), each with all
#                         the jobs; the archives are the same as the
#                         concurrent mode's
#   KEEP_TREES=1          keep the temporary worktrees after the run
#   SERIAL=1              passed on to archive-pass.sh: its original one
#                         'make <group>.group' at a time mode
#   GATE_TIMING=FILE      a timing table for archive-pass.sh (slowest test
#                         directories first).  Default: <archive-root>/
#                         timing.txt, which every run leaves behind from its
#                         normal pass, so a second run in an archive root is
#                         scheduled by the first one's times; when there is
#                         none, the committed util/determinism/timing.txt.
#                         The table is copied to <archive-root>/timing.txt,
#                         which is where the passes read it
#   TEST_RELEASE, GATE_OUT   defaults for the two positional arguments (CI
#                         sets these and passes no arguments)
#
# A lock directory <archive-root>/lock (holding the gate's pid) keeps two
# gates from sharing an archive root; a lock whose pid is gone is taken
# over.  On SIGINT or SIGTERM the gate kills both passes (their process
# groups), removes the worktrees unless KEEP_TREES=1, releases the lock
# and exits 128 + the signal number.
set -u -o pipefail

die() { printf 'run-gate.sh: %s\n' "$*" >&2; exit 3; }
usage() { die 'usage: run-gate.sh [inst-dir] [archive-root] [compare.py options...]'; }
say() { echo "== run-gate: $*"; }
load_now() { uptime 2>/dev/null | sed -n 's/.*load average[s]*: */load /p'; }

here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P) || die 'cannot locate myself'
worktree=$(cd "$here/../.." && pwd -P) || die 'cannot locate the checkout'

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
command -v setsid > /dev/null || die 'setsid not found'
git -C "$worktree" rev-parse --git-dir > /dev/null 2>&1 || die "$worktree is not a git checkout"
ncpu=$(nproc 2>/dev/null || getconf _NPROCESSORS_ONLN 2>/dev/null || echo 2)
jobs=${JOBS:-$ncpu}
[ "$jobs" -ge 1 ] 2>/dev/null || die "JOBS must be a positive integer, not '$jobs'"
sequential=${SEQUENTIAL:-0}
keep_trees=${KEEP_TREES:-0}

# Fail now, not two suite runs later, if this bsc does not know the flags.
scratch=$(mktemp -d "${TMPDIR:-/tmp}/run-gate.XXXXXX") || die 'mktemp failed'
printf 'package Empty where\n' > "$scratch/Empty.bs"
if ! "$inst/bin/bsc" -reverse-intern-order -remap-path-prefix "$scratch=." -bdir "$scratch" "$scratch/Empty.bs" > "$scratch/bsc.out" 2>&1; then
  cat "$scratch/bsc.out" >&2
  rm -rf "$scratch"
  die "$inst/bin/bsc does not accept -reverse-intern-order and -remap-path-prefix"
fi
rm -rf "$scratch"

mkdir -p "$root" || die "cannot create '$root'"
root=$(cd "$root" && pwd -P) || die "cannot enter '$root'"
[ "$root" != "$worktree" ] || die "the archive root cannot be the checkout itself"
case $root/ in
  "$worktree"/testsuite/*) die "archive root '$root' is inside the checkout's testsuite" ;;
esac
case $inst/ in
  "$root"/trees/*) die "installation '$inst' is under $root/trees, which the gate removes" ;;
esac

# ------------------------------------------------------------------ the lock
lock=$root/lock
locked=0
unlock() { [ "$locked" = 1 ] && rm -rf "$lock"; locked=0; }
take_lock() {
  local holder
  if mkdir "$lock" 2>/dev/null; then
    echo $$ > "$lock/pid"; locked=1; return 0
  fi
  holder=$(cat "$lock/pid" 2>/dev/null)
  if [ -n "$holder" ] && kill -0 "$holder" 2>/dev/null; then
    die "another gate (pid $holder) holds $lock; use a different archive root or wait for it"
  fi
  say "taking over the stale lock $lock (pid ${holder:-unknown} is gone)"
  rm -rf "$lock"
  mkdir "$lock" || die "cannot create $lock"
  echo $$ > "$lock/pid"; locked=1
}

# ------------------------------------------------------------- the worktrees
# Only the two trees this script creates are ever removed; anything else
# under <archive-root>/trees/ is left alone.
trees_live=0
remove_trees() {
  local t
  for t in "$root/trees/normal" "$root/trees/reversed"; do
    [ -e "$t" ] || [ -L "$t" ] || continue
    git -C "$worktree" worktree remove --force "$t" > /dev/null 2>&1 || rm -rf "$t"
  done
  git -C "$worktree" worktree prune > /dev/null 2>&1
  rmdir "$root/trees" 2>/dev/null
  trees_live=0
}
add_tree() {  # add_tree <name> <log>: a detached worktree of HEAD at trees/<name>
  mkdir -p "$root/trees" || die "cannot create $root/trees"
  git -C "$worktree" worktree add --detach "$root/trees/$1" HEAD > "$2" 2>&1 \
    || { cat "$2"; remove_trees; die "git worktree add $root/trees/$1 failed"; }
  trees_live=1
}

# -------------------------------------------------------- passes and signals
pids=()
start_pass() {  # start_pass <name> <tree> <jobs> <TEST_BSC_OPTIONS>: in the background
  local name=$1 tree=$2 n=$3 opts=$4
  GATE_WORKTREE=$tree JOBS=$n setsid "$here/archive-pass.sh" "$inst" "$root/$name" "$opts" \
    >> "$root/pass-$name.log" 2>&1 &
  pids+=($!)
}
stop_passes() {  # TERM to each pass's process group, then KILL what is left
  local p i alive
  [ ${#pids[@]} -gt 0 ] || return 0
  for p in "${pids[@]}"; do kill -TERM -- "-$p" 2>/dev/null; done
  for i in 1 2 3 4 5 6 7 8 9 10; do
    alive=0
    for p in "${pids[@]}"; do kill -0 "$p" 2>/dev/null && alive=1; done
    [ "$alive" = 1 ] || break
    sleep 1
  done
  for p in "${pids[@]}"; do kill -KILL -- "-$p" 2>/dev/null; done
  wait 2>/dev/null
  pids=()
}
on_signal() {  # on_signal <name> <number>
  trap '' INT TERM
  say "caught SIG$1: stopping the passes$([ "$keep_trees" = 1 ] || echo ' and removing the worktrees')  $(date '+%F %T')"
  stop_passes
  [ "$keep_trees" = 1 ] || remove_trees
  unlock
  trap - EXIT
  exit $((128 + $2))
}
on_exit() {  # a die() on the way out must not leave trees or the lock behind
  stop_passes
  [ "$trees_live" = 0 ] || [ "$keep_trees" = 1 ] || remove_trees
  unlock
}
trap 'on_signal INT 2' INT
trap 'on_signal TERM 15' TERM
trap on_exit EXIT

pass_status() {  # pass_status <name> <exit>: report a pass's exit; 0 when fine
  local name=$1 st=$2
  case $st in
    0) ;;
    4) say "$name: INCOMPLETE: expected test directories wrote no testrun.sum (see pass-$name.log and $root/$name/_log/no_sum.txt)" ;;
    *) say "$name: archive-pass.sh exited $st (see pass-$name.log)" ;;
  esac
  return "$st"
}

# ----------------------------------------------------------------- the run
take_lock
remove_trees
# The timing table the passes read is <archive-root>/timing.txt.  An
# explicit GATE_TIMING, or the committed table when the root has none yet,
# is copied there before the previous run's outputs (which may hold it) go.
timing=${GATE_TIMING:-}
if [ -z "$timing" ]; then
  if [ -s "$root/timing.txt" ]; then timing=$root/timing.txt
  elif [ -s "$here/timing.txt" ]; then timing=$here/timing.txt
  fi
fi
if [ -n "$timing" ]; then
  [ -s "$timing" ] || die "GATE_TIMING '$timing' is not a non-empty file"
  timing=$(cd "$(dirname "$timing")" && pwd -P)/$(basename "$timing") || die "cannot resolve GATE_TIMING '$timing'"
  if [ "$timing" != "$root/timing.txt" ]; then
    cp "$timing" "$root/timing.txt" || die "cannot copy '$timing' to $root/timing.txt"
  fi
  export GATE_TIMING=$root/timing.txt
else
  export GATE_TIMING=
fi
rm -rf "$root/normal" "$root/reversed" "$root"/pass-*.log "$root"/compare.*

gate_t0=$(date +%s)
say "checkout $worktree  inst $inst  archives $root  $(date '+%F %T')  $(load_now)"
# The allow-list is a baseline for one bsc build (see the header of
# allowlist.txt); say which build this is so a mismatch is visible in the log.
say "$("$inst/bin/bsc" -v 2>&1 | head -n 1)"
say "commit $(git -C "$worktree" rev-parse --short HEAD 2>/dev/null || echo '?')  jobs $jobs$([ "${SERIAL:-0}" = 1 ] && echo '  SERIAL=1 (one group at a time inside each pass)')"
say "timing table: ${timing:-none (tests start in directory order)}"
if [ -n "${ONLY_GROUPS:-}" ]; then
  say "ONLY_GROUPS='$ONLY_GROUPS'"
  compare_opts+=(--scope-groups "$ONLY_GROUPS")
fi
if [ -n "$(git -C "$worktree" status --porcelain --untracked-files=no -- testsuite 2>/dev/null)" ]; then
  say "WARNING: uncommitted changes under testsuite/ are not in the temporary worktrees (they check out HEAD)"
fi

remap() { echo "-remap-path-prefix $1=."; }
if [ "$sequential" = 1 ]; then
  tree=$root/trees/normal
  add_tree normal "$root/pass-normal.log"
  say "sequential: two passes one after the other in $tree, $jobs jobs each"
  say "TEST_BSC_OPTIONS: normal '$(remap "$tree")'  reversed '-reverse-intern-order $(remap "$tree")'"
  t0=$(date +%s)
  start_pass normal "$tree" "$jobs" "$(remap "$tree")"
  wait "${pids[0]}"; st_normal=$?; t1=$(date +%s)
  cat "$root/pass-normal.log"
  pids=()
  : > "$root/pass-reversed.log"
  start_pass reversed "$tree" "$jobs" "-reverse-intern-order $(remap "$tree")"
  wait "${pids[0]}"; st_reversed=$?; t2=$(date +%s)
  cat "$root/pass-reversed.log"
  pids=()
  say "pass wall times: normal $((t1 - t0))s  reversed $((t2 - t1))s"
else
  half=$(( (jobs + 1) / 2 ))
  add_tree normal "$root/pass-normal.log"
  add_tree reversed "$root/pass-reversed.log"
  say "concurrent: two passes in $root/trees/{normal,reversed}, $half jobs each"
  say "TEST_BSC_OPTIONS: normal '$(remap "$root/trees/normal")'  reversed '-reverse-intern-order $(remap "$root/trees/reversed")'"
  say "pass logs: $root/pass-normal.log  $root/pass-reversed.log"
  t0=$(date +%s)
  start_pass normal "$root/trees/normal" "$half" "$(remap "$root/trees/normal")"
  start_pass reversed "$root/trees/reversed" "$half" "-reverse-intern-order $(remap "$root/trees/reversed")"
  wait "${pids[0]}"; st_normal=$?; t1=$(date +%s)
  wait "${pids[1]}"; st_reversed=$?; t2=$(date +%s)
  pids=()
  cat "$root/pass-normal.log" "$root/pass-reversed.log"
  say "pass wall times: normal $((t1 - t0))s  reversed $((t2 - t0))s (both started together)"
fi
if [ "$keep_trees" = 1 ]; then
  say "KEEP_TREES=1: worktrees kept under $root/trees/"
else
  remove_trees
fi
incomplete=0
pass_status normal "$st_normal"; st=$?
case $st in 0) ;; 4) incomplete=1 ;; *) die "archive-pass.sh (normal) failed" ;; esac
pass_status reversed "$st_reversed"; st=$?
case $st in 0) ;; 4) incomplete=1 ;; *) die "archive-pass.sh (reversed) failed" ;; esac

# keep the normal pass's timing table for the next run in this archive root
[ -s "$root/normal/_log/timing.txt" ] && cp "$root/normal/_log/timing.txt" "$root/timing.txt"

say "comparing  $(date '+%F %T')"
python3 "$here/compare.py" "$root/normal" "$root/reversed" \
  --allowlist "$allowlist" --ignore "$ignore" --out "$root/compare" \
  ${compare_opts[@]+"${compare_opts[@]}"}
status=$?
if [ "$incomplete" = 1 ]; then
  say "RESULT: INCOMPLETE: a pass wrote no testrun.sum for some of its test directories, so the comparison above is not a verdict (compare.py exit $status); exit 3"
  status=3
fi
say "done, exit $status  $(( $(date +%s) - gate_t0 ))s wall  $(load_now)  $(date '+%F %T')"
exit "$status"
