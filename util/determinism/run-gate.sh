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
# (a user namespace's root would read them).  The installation and the
# archive root may be under this checkout (the defaults are, and so is CI's
# layout): the mount would hide them, so the real checkout is first bound
# to an alias directory and each such directory is then bound back from
# the alias to its own path, over the temporary worktree (so are the
# directories named in GATE_PASSTHROUGH).  A pre-flight probe tries exactly
# that, pass-throughs included; where it does not work (no unprivileged
# user namespaces), the passes run one after the other in this checkout
# with all the jobs, which SEQUENTIAL=1 asks for explicitly.
#
#   <inst-dir>      the bsc installation to test; defaults to $TEST_RELEASE,
#                   then <worktree>/inst
#   <archive-root>  defaults to $GATE_OUT, then <worktree>/build/determinism;
#                   a previous run's normal/, reversed/, trees/, pass-*.log
#                   and compare.* there are removed first.  Neither may be
#                   the checkout itself or anything under its testsuite/
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
#                         first one's times.  A table named explicitly is
#                         copied to <archive-root>/timing.txt first, which is
#                         where the passes read it
#   GATE_PASSTHROUGH="d1 d2"  further directories under this checkout that
#                         the concurrent passes must see as they really are
#                         (a CCACHE_DIR inside the checkout, say); created if
#                         missing.  The installation and the archive root
#                         are always passed through when they are inside
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
[ "$root" != "$worktree" ] || die "the archive root cannot be the checkout itself"
[ "$inst" != "$worktree" ] || die "the installation cannot be the checkout itself"
# Under testsuite/ the pass's 'git clean' would delete them (through the
# pass-through, the real files).
case $root/ in
  "$worktree"/testsuite/*) die "archive root '$root' is inside the testsuite" ;;
esac
case $inst/ in
  "$worktree"/testsuite/*) die "installation '$inst' is inside the testsuite" ;;
esac

# The directories under this checkout that the concurrent passes must see
# as they really are, outermost first (a later mount over an earlier one
# only re-exposes what the earlier one already showed).
passthrough=()
add_passthrough() {  # add_passthrough <dir>: record it when it is inside the checkout
  local p=$1 q i
  case $p/ in "$worktree"/*) ;; *) return 0 ;; esac
  for q in ${passthrough[@]+"${passthrough[@]}"}; do [ "$q" != "$p" ] || return 0; done
  passthrough+=("$p")
  # keep the list sorted by path length
  for ((i = ${#passthrough[@]} - 1; i > 0; i--)); do
    [ ${#passthrough[i]} -lt ${#passthrough[i-1]} ] || break
    q=${passthrough[i]}; passthrough[i]=${passthrough[i-1]}; passthrough[i-1]=$q
  done
}
add_passthrough "$inst"
add_passthrough "$root"
for p in ${GATE_PASSTHROUGH:-}; do
  # checked as given before anything is created, and in canonical form after
  case $p in /*) ;; *) p=$PWD/$p ;; esac
  case $p/ in "$worktree"/testsuite/*) die "GATE_PASSTHROUGH: '$p' is inside the testsuite" ;; esac
  mkdir -p "$p" || die "GATE_PASSTHROUGH: cannot create '$p'"
  p=$(cd "$p" && pwd -P) || die "GATE_PASSTHROUGH: cannot enter '$p'"
  [ "$p" != "$worktree" ] || die "GATE_PASSTHROUGH: '$p' is the checkout itself"
  case $p/ in
    "$worktree"/testsuite/*) die "GATE_PASSTHROUGH: '$p' is inside the testsuite" ;;
    "$worktree"/*) add_passthrough "$p" ;;
    *) say "GATE_PASSTHROUGH: $p is not under $worktree, nothing to do" ;;
  esac
done

# in_tree_at <tree> <path> <cmd...>: run <cmd> in a private mount namespace
# with <tree> bind-mounted over <path>, and every directory in $passthrough
# (all under <path>) still showing its real contents: the real <path> is
# bound to $alias first, and each pass-through directory is then bound from
# there over its own path (created in <tree> when it does not exist there;
# the git-ignored inst/ and build/, usually).  Only the creator of a user
# namespace may mount in it, so unshare maps us to its root; setpriv then
# empties the bounding and inheritable sets, so that nothing exec'd after
# it holds a capability (uid 0 with none behaves as an ordinary owner
# towards file modes).  97 is a mount's own failure.
in_tree_at() {
  local tree=$1 path=$2; shift 2
  unshare --user --map-root-user --mount -- sh -c '
    tree=$1 path=$2 alias=$3 n=$4; shift 4
    mount --bind "$path" "$alias" || exit 97
    mount --bind "$tree" "$path" || exit 97
    while [ "$n" -gt 0 ]; do
      p=$1; shift; n=$((n - 1))
      mkdir -p "$p" && mount --bind "$alias/${p#"$path"/}" "$p" || exit 97
    done
    exec setpriv --bounding-set=-all --inh-caps=-all -- "$@"' \
    in_tree_at "$tree" "$path" "$alias" "${#passthrough[@]}" ${passthrough[@]+"${passthrough[@]}"} "$@"
}

# Does in_tree_at work here?  Checks what the passes rely on: the tree shows
# at the path, also through pwd -P (the paths bsc records come from the
# physical cwd), a mode-000 file is unreadable inside, every pass-through
# directory is the real one (same device and inode as outside), the
# installation's bsc is there and the archive root takes a write.  Prints
# the reason when not.
probe_sandbox() {
  local d=$root/probe out ids=() p
  command -v unshare > /dev/null || { echo 'unshare not found'; return 1; }
  command -v setpriv > /dev/null || { echo 'setpriv not found'; return 1; }
  rm -rf "$d"; mkdir -p "$d/tree" || { echo "cannot create $d"; return 1; }
  echo probe > "$d/tree/gate-probe"
  : > "$d/tree/locked"; chmod 000 "$d/tree/locked"
  for p in ${passthrough[@]+"${passthrough[@]}"}; do
    ids+=("$p=$(stat -c %d:%i "$p" 2>/dev/null || echo '?')")
  done
  out=$(in_tree_at "$d/tree" "$worktree" sh -c '
    path=$1 inst=$2 probe=$3; shift 3
    cd "$path" || { echo "cannot cd to $path"; exit 1; }
    [ "$(pwd -P)" = "$path" ] || { echo "pwd -P is $(pwd -P), not $path"; exit 1; }
    [ -f gate-probe ] || { echo "the bind mount is not visible at $path"; exit 1; }
    if cat locked > /dev/null 2>&1; then echo "a mode-000 file is readable inside the namespace"; exit 1; fi
    for pair in "$@"; do
      p=${pair%=*}; id=${pair##*=}
      [ "$(stat -c %d:%i "$p" 2>/dev/null)" = "$id" ] || { echo "$p inside the namespace is not the real directory"; exit 1; }
    done
    [ -x "$inst/bin/bsc" ] || { echo "$inst/bin/bsc is not visible inside the namespace"; exit 1; }
    echo inside > "$probe/written-inside" 2>/dev/null || { echo "cannot write to $probe inside the namespace"; exit 1; }
    echo ok' probe "$worktree" "$inst" "$d" ${ids[@]+"${ids[@]}"} 2>&1)
  if [ "$out" = ok ] && [ ! -f "$d/written-inside" ]; then
    out="a file written to $d inside the namespace did not land there"
  fi
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
# The timing table the passes read is <archive-root>/timing.txt: a table
# named explicitly is copied there (an archive root inside the checkout is
# passed through to the concurrent passes, an arbitrary path inside it is
# not), before the previous run's outputs, which may hold it, go.
timing=${GATE_TIMING:-$root/timing.txt}
if [ -s "$timing" ]; then
  timing=$(cd "$(dirname "$timing")" && pwd -P)/$(basename "$timing") || die "cannot resolve GATE_TIMING '$GATE_TIMING'"
  if [ "$timing" != "$root/timing.txt" ]; then
    cp "$timing" "$root/timing.txt" || die "cannot copy GATE_TIMING '$timing' to $root/timing.txt"
    timing=$root/timing.txt
  fi
else
  timing=
fi
export GATE_TIMING=$timing
rm -rf "$root/normal" "$root/reversed" "$root"/pass-*.log "$root"/compare.* "$root/archive-pass.sh"

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

alias=
if [ "$sequential" != 1 ]; then
  # The alias of the real checkout for in_tree_at: a mount point only, so
  # it stays empty on disk.  Both passes bind to it in their own namespaces.
  alias=$(mktemp -d "${TMPDIR:-/tmp}/run-gate-alias.XXXXXX") || die 'mktemp failed'
  trap 'rmdir "$alias" 2> /dev/null' EXIT
  case $alias/ in
    "$worktree"/*) die "TMPDIR ($alias) is inside the checkout; set it elsewhere" ;;
  esac
  if reason=$(probe_sandbox); then
    say "same-path sandbox: ok (unshare --user --mount + setpriv)"
    [ ${#passthrough[@]} -eq 0 ] || say "passed through (under $worktree, seen as they are): ${passthrough[*]}"
  else
    say "same-path sandbox unavailable: $reason"
    say "running the passes one after the other in $worktree instead"
    sequential=1
  fi
fi

pass_status() {  # pass_status <name> <exit>: report a pass's exit, 0 when fine
  local name=$1 st=$2
  if [ "$st" -eq 97 ]; then
    say "$name: a bind mount failed inside the namespace (see pass-$name.log)"
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
