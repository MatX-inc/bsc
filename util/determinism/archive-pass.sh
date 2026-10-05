#!/usr/bin/env bash
# archive-pass.sh -- run the bsc testsuite once and archive every file it
# generates.
#
# usage: archive-pass.sh <inst-dir> <archive-dir> [TEST_BSC_OPTIONS value]
#
#   <inst-dir>     a bsc installation; passed to make as TEST_RELEASE
#   <archive-dir>  where the generated files land, with their paths relative
#                  to testsuite/ preserved.  _sum/ holds every testrun.sum the
#                  run wrote, under its directory's path, and one <group>.sum
#                  per group (the sums of that group's test directories, in
#                  order: the per-group layout the serial mode writes); _log/
#                  holds the make transcript, the group -> .exp map and the
#                  TESTDIRS value
#   [options]      the TEST_BSC_OPTIONS value, one argument, e.g.
#                  '-reverse-intern-order -remap-path-prefix /x/tree=.'
#
# environment:
#   ONLY_GROUPS="a b c"   run only these top-level groups
#   JOBS=N                parallel mode: 'make -jN checkparallel' (default:
#                         half the CPUs, because run-gate.sh runs two passes
#                         at once)
#   SERIAL=1              the original mode: one 'make <group>.group' at a
#                         time, serial inside too, with a clean between
#                         groups.  Same archive, about ten times slower; kept
#                         for comparison
#   GATE_WORKTREE=DIR     the checkout whose testsuite to run (default: the
#                         one this script lives in; run-gate.sh names its
#                         temporary worktrees here)
#   GATE_TIMING=FILE      parallel mode: a timing table from an earlier run
#                         (the _log/timing.txt every parallel pass writes);
#                         with one, the slowest test directories start first
#                         (RUN_TESTCASES_IN_ORDER_OF_TIME, scripts/
#                         sort-by-time.pl), which shortens the tail of the
#                         run; without one they start in directory order
#
# Groups are the top-level entries of 'make groups.list' minus the aggregates
# ALL, LONG and dev.  'make <group>.group' hands runtest the basenames in
# $(<group>.list) (test_list.sh: every .exp under bsc.<group>/, plus the
# Makefile's own long_tests.list), and dejagnu then runs EVERY .exp of that
# basename under the bsc.* directories, so a group also runs the same-named
# .exp files of other groups: scheduler runs
# bsc.names/portRenaming/prefixTests/methods/methods.exp, verilog runs
# bsc.bluesim/vcd/vcd.exp and seven more.  The parallel mode resolves each
# group the same way (group_exps below) and passes the resolved .exp paths as
# TESTDIRS, so ONLY_GROUPS means the same set of tests in both modes; without
# ONLY_GROUPS every .exp runs once, which is what every group together did.
#
# Parallel mode: 'make -j$JOBS checkparallel' (testsuite/suitemake.mk) runs
# every .exp from its own directory, as the main CI does, with
# DO_INTERNAL_CHECKS=1, TEST_RELEASE=<inst-dir> and TEST_BSC_OPTIONS as
# given (suitemake.mk's RUNTESTENV turns it into BSC_OPTIONS on that path
# too).  The tree is cleaned with 'git clean -fdXq .' before and after.
# Archived: every git-ignored (hence generated) regular file except objects,
# archives, executables (*.cexe *.vexe *.syscexe and ELF binaries), *.log,
# *.sum, and the parallel driver's own bookkeeping (all_tests.mk, timing.txt).
# Test failures never make this script exit non-zero (the gate compares
# bytes; test results are the other CI's job).  The exit status is 2 for a
# usage or I/O error and, in parallel mode, 4 when a test directory the run
# was expected to cover wrote no testrun.sum (its runtest did not finish, so
# the archive is incomplete: a comparison would report its files absent);
# the directories are listed in _log/no_sum.txt and the archive is kept.
set -u

die() { printf 'archive-pass.sh: %s\n' "$*" >&2; exit 2; }
usage() { die 'usage: archive-pass.sh <inst-dir> <archive-dir> [TEST_BSC_OPTIONS value]'; }

[ $# -ge 2 ] && [ $# -le 3 ] || usage
inst=$1; archive=$2; bsc_options=${3:-}

script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P) || die 'cannot locate myself'
if [ -n "${GATE_WORKTREE:-}" ]; then
  worktree=$(cd "$GATE_WORKTREE" && pwd -P) || die "cannot enter GATE_WORKTREE '$GATE_WORKTREE'"
else
  worktree=$(cd "$script_dir/../.." && pwd -P) || die 'cannot locate the worktree'
fi
suite=$worktree/testsuite
[ -f "$suite/Makefile" ] && [ -f "$suite/test_list.sh" ] || die "no testsuite at '$suite'"

[ -x "$inst/bin/bsc" ] || die "no bsc installation at '$inst' (expected $inst/bin/bsc)"
inst=$(cd "$inst" && pwd -P) || die "cannot enter '$inst'"

serial=${SERIAL:-0}
ncpu=$(nproc 2>/dev/null || getconf _NPROCESSORS_ONLN 2>/dev/null || echo 2)
jobs=${JOBS:-$(( ncpu / 2 ))}
[ "$jobs" -ge 1 ] 2>/dev/null || jobs=1

# The archive must not live under the testsuite: git clean would delete it.
# Check the path as given before creating anything, and its canonical form after.
case $archive in /*) ;; *) archive=$PWD/$archive ;; esac
case $archive/ in
  "$suite"/*) die "archive dir '$archive' is inside the testsuite" ;;
esac
mkdir -p "$archive/_sum" "$archive/_log" || die "cannot create '$archive'"
archive=$(cd "$archive" && pwd -P) || die "cannot enter '$archive'"
case $archive/ in
  "$suite"/*) rmdir "$archive/_sum" "$archive/_log" "$archive" 2> /dev/null
              die "archive dir '$archive' is inside the testsuite" ;;
esac
cd "$suite" || die "cannot enter '$suite'"

git clean -fdXq . || die "git clean failed in $suite"
make groups.list > /dev/null 2>&1 || die "'make groups.list' failed in $suite"
# Top-level groups: every X.list in groups.list except the aggregates and the
# per-subdirectory lists (bsc.foo/bar.list), which the top-level groups cover.
all_groups=$(sed -nE 's/^([A-Za-z0-9_-]+)\.list[[:space:]]*=.*/\1/p' groups.list \
             | grep -vxE 'ALL|LONG|dev' | sort -u)
[ -n "$all_groups" ] || die "no groups found in $suite/groups.list"

if [ -n "${ONLY_GROUPS:-}" ]; then
  group_list=
  for g in $ONLY_GROUPS; do
    printf '%s\n' "$all_groups" | grep -qx -- "$g" \
      || die "unknown group '$g' in ONLY_GROUPS (known: $(printf '%s ' $all_groups))"
    group_list="$group_list $g"
  done
else
  group_list=$all_groups
fi

# The basenames 'make <group>.group' hands to runtest: $(<group>.list) as the
# Makefile sees it (groups.list, overridden by the Makefile for long_tests).
group_names() {
  make -s --eval 'gate-list-%: ; @echo $($*.list)' "gate-list-$1" 2> /dev/null
}

# The .exp files 'make <group>.group' runs, one relative path per line,
# sorted: every file named <basename>.exp under the bsc.* directories, which
# is how dejagnu resolves a basename on its command line (runtest.exp: find
# over the tool's top directories, exact match on [file tail]).  Names with
# no file are reported on stderr and otherwise ignored, as runtest does.
group_exps() {
  local names n missing=
  names=$(group_names "$1") || die "cannot evaluate \$($1.list)"
  for n in $(printf '%s\n' $names | sort -u); do
    case $n in */*|*.*|'') die "unexpected test name '$n' in \$($1.list)" ;; esac
    if ! find bsc.*/ -type f -name "$n.exp" -print | grep -q .; then
      missing="$missing $n"
      continue
    fi
    find bsc.*/ -type f -name "$n.exp" -print
  done | sort -u
  [ -z "$missing" ] || printf 'archive-pass.sh: note: %s.list names tests with no .exp file:%s\n' "$1" "$missing" >&2
}

# Print, NUL-separated, the generated files worth archiving: every git-ignored
# regular file except objects, archives, executables and logs.  *.sum is
# handled separately (per-directory sums -> _sum/).  The top-level
# all_tests.mk and timing.txt* are the parallel driver's own bookkeeping
# (the test list and the timing table generate-stats refreshes), not test
# output; the serial mode never creates them.
generated_files() {
  local f magic
  git ls-files --others --ignored --exclude-standard -z | while IFS= read -r -d '' f; do
    [ -f "$f" ] && [ ! -L "$f" ] || continue
    case $f in
      *.o|*.so|*.a|*.dylib|*.log|*.sum|*.cexe|*.vexe|*.syscexe|*.exe|*.dSYM/*) continue ;;
      all_tests.mk|timing.txt|timing.txt.*) continue ;;
    esac
    if [ -x "$f" ]; then
      # keep executable text (the vvp dumps *.inline-reg are copies of a
      # .vexe and inherit its mode); drop compiled binaries
      magic=$(head -c 4 -- "$f" 2>/dev/null)
      [ "$magic" = $'\x7fELF' ] && continue
    fi
    printf '%s\0' "$f"
  done
}

# Copy the generated files of the current tree into the archive and leave the
# NUL-separated list in $1; print how many.
archive_tree() {
  local count
  generated_files > "$1"
  count=$(tr -cd '\0' < "$1" | wc -c)
  xargs -0 -r cp --parents -t "$archive/" < "$1"
  echo "$count"
}

count_results() {  # count_results <kind-regex> <sum-file>: how many such lines
  local n
  n=$(grep -cE "^($1):" "$2" 2>/dev/null)
  echo "${n:-0}"
}

load_now() { uptime 2>/dev/null | sed -n 's/.*load average[s]*: */load /p'; }

commit=$(git -C "$worktree" rev-parse --short HEAD 2>/dev/null || echo '?')
mode=$([ "$serial" = 1 ] && echo 'serial, one make <group>.group at a time' || echo "parallel, make -j$jobs checkparallel")
echo "== archive-pass: $commit  inst=$inst  TEST_BSC_OPTIONS='$bsc_options'  archive=$archive  $(date '+%F %T')"
echo "== archive-pass: $mode  worktree=$worktree  $(load_now)"
pass_t0=$(date +%s)

# ---------------------------------------------------------------- serial mode
if [ "$serial" = 1 ]; then
  total_pass=0; total_fail=0; total_unres=0; total_files=0
  for g in $group_list; do
    t0=$(date +%s)
    DO_INTERNAL_CHECKS=1 TEST_BSC_OPTIONS="$bsc_options" \
      make TEST_RELEASE="$inst" "$g.group" > "$archive/_log/$g.log" 2>&1
    make_status=$?
    if [ -f bsc.sum ]; then
      cp bsc.sum "$archive/_sum/$g.sum"
      pass=$(count_results 'PASS' bsc.sum); fail=$(count_results 'FAIL' bsc.sum)
      unres=$(count_results 'UNRESOLVED|ERROR' bsc.sum)
    else
      pass=0; fail=0; unres=0
    fi
    list=$(mktemp "${TMPDIR:-/tmp}/archive-pass.XXXXXX") || die 'mktemp failed'
    n=$(archive_tree "$list")
    rm -f "$list"
    # runtest exits non-zero whenever a test fails, which the counts already
    # show; flag the exit status only when it comes with no results at all
    note=
    [ -f bsc.sum ] || note="  (no bsc.sum)"
    [ "$make_status" -eq 0 ] || [ $((pass + fail + unres)) -gt 0 ] || note="$note  (make exit $make_status)"
    printf '   %-14s pass %5d fail %4d unresolved %3d  archived %6d files  %5ds%s\n' \
      "$g" "$pass" "$fail" "$unres" "$n" $(( $(date +%s) - t0 )) "$note"
    total_pass=$((total_pass + pass)); total_fail=$((total_fail + fail))
    total_unres=$((total_unres + unres)); total_files=$((total_files + n))
    git clean -fdXq . || die "git clean failed in $suite"
  done
  echo "== archive-pass done: pass $total_pass fail $total_fail unresolved $total_unres  archived $total_files files  $(( $(date +%s) - pass_t0 ))s wall  $(load_now)  $(date '+%F %T')"
  exit 0
fi

# -------------------------------------------------------------- parallel mode
tmp=$(mktemp -d "${TMPDIR:-/tmp}/archive-pass.XXXXXX") || die 'mktemp failed'
trap 'rm -rf "$tmp"' EXIT

# Resolve every group to its .exp files; record the map for audit.
: > "$archive/_log/groups.map"
for g in $group_list; do
  group_exps "$g" > "$tmp/$g.exps" 2> "$tmp/$g.note"
  [ -s "$tmp/$g.exps" ] || die "group '$g' resolves to no .exp file"
  cat "$tmp/$g.note" >&2
  sed "s/^/$g /" "$tmp/$g.exps" >> "$archive/_log/groups.map"
  sed 's/^archive-pass.sh: note: /# /' "$tmp/$g.note" >> "$archive/_log/groups.map"
done
# No testsuite directory holds two .exp files (scripts/double-directory.pl
# enforces it), so a test's directory stands for the test.
for g in $group_list; do sed 's|/[^/]*$||' "$tmp/$g.exps"; done | sort -u > "$tmp/exp_dirs"
if [ -n "${ONLY_GROUPS:-}" ]; then
  # TESTDIRS entries are matched whole or as a directory prefix
  # (scripts/filter-testdirs.pl); the .exp paths themselves select exactly
  # the resolved tests and nothing below a directory such as bsc.verilog/
  # that also holds other groups' tests.
  testdirs=$(cat "$tmp"/*.exps | sort -u | tr '\n' ' ')
else
  testdirs=
fi
printf '%s\n' "$testdirs" > "$archive/_log/testdirs.txt"
n_exp=$(wc -l < "$tmp/exp_dirs")
echo "== archive-pass: $n_exp .exp files in $(printf '%s\n' $group_list | wc -l) groups$([ -n "$testdirs" ] && echo ' (TESTDIRS set)' || echo ' (every .exp)')"

# The timing table goes in after the git clean above (it is git-ignored) and
# survives checkparallel's own 'make clean', which does not touch it.
order=
if [ -n "${GATE_TIMING:-}" ] && [ -s "$GATE_TIMING" ]; then
  cp "$GATE_TIMING" timing.txt || die "cannot copy GATE_TIMING '$GATE_TIMING'"
  export RUN_TESTCASES_IN_ORDER_OF_TIME=1
  order=" (slowest first, from $GATE_TIMING)"
fi
echo "== archive-pass: make -j$jobs checkparallel$order  $(date '+%F %T')"
t0=$(date +%s)
DO_INTERNAL_CHECKS=1 TEST_BSC_OPTIONS="$bsc_options" TESTDIRS="$testdirs" \
  make -j"$jobs" TEST_RELEASE="$inst" checkparallel > "$archive/_log/checkparallel.log" 2>&1
make_status=$?
make_secs=$(( $(date +%s) - t0 ))
# generate-stats leaves the merged timing table for the next run
[ -f timing.txt ] && cp timing.txt "$archive/_log/timing.txt"

# The sums: every testrun.sum under its directory's path, and the group view.
find . -name '*.sum' -type f -print | sed 's|^\./||' | sort > "$tmp/sums"
while IFS= read -r s; do
  mkdir -p "$archive/_sum/$(dirname "$s")" && cp "$s" "$archive/_sum/$s"
done < "$tmp/sums"
sed 's|/[^/]*$||' "$tmp/sums" | sort -u > "$tmp/sum_dirs"
comm -23 "$tmp/exp_dirs" "$tmp/sum_dirs" > "$tmp/no_sum"
comm -13 "$tmp/exp_dirs" "$tmp/sum_dirs" > "$tmp/extra_sum"

list=$tmp/files
n_files=$(archive_tree "$list")
# Count archived files per test directory: a file belongs to the nearest
# enclosing directory that holds a .exp (bsc.verilog/instports/x.v is
# instports', not bsc.verilog/verilog.exp's).
tr '\0' '\n' < "$list" | awk -v dirs="$tmp/exp_dirs" '
  BEGIN { while ((getline d < dirs) > 0) owner[d] = 1 }
  { p = $0
    while (!(p in owner)) { if (p !~ /\//) { p = ""; break }; sub(/\/[^\/]*$/, "", p) }
    if (p != "") n[p]++ }
  END { for (d in n) print d "\t" n[d] }' > "$tmp/dir_counts"

total_pass=0; total_fail=0; total_unres=0
echo "   (test-time: the sum of the group's own timed test commands from time.out, not wall time)"
for g in $group_list; do
  : > "$archive/_sum/$g.sum"
  files=0; secs=0; missing=0
  while IFS= read -r e; do
    d=${e%/*}
    if [ -f "$d/testrun.sum" ]; then
      cat "$d/testrun.sum" >> "$archive/_sum/$g.sum"
    else
      missing=$((missing + 1))
    fi
    c=$(awk -F'\t' -v d="$d" '$1 == d { print $2 }' "$tmp/dir_counts")
    files=$((files + ${c:-0}))
    if [ -f "$d/time.out" ]; then
      s=$(awk -F', *' 'NF >= 4 { t += $4 } END { printf "%d", t }' "$d/time.out")
      secs=$((secs + ${s:-0}))
    fi
  done < "$tmp/$g.exps"
  pass=$(count_results 'PASS' "$archive/_sum/$g.sum"); fail=$(count_results 'FAIL' "$archive/_sum/$g.sum")
  unres=$(count_results 'UNRESOLVED|ERROR' "$archive/_sum/$g.sum")
  note=
  [ "$missing" -eq 0 ] || note="  ($missing of $(wc -l < "$tmp/$g.exps") test dirs wrote no testrun.sum)"
  printf '   %-14s pass %5d fail %4d unresolved %3d  archived %6d files  test-time %6ds%s\n' \
    "$g" "$pass" "$fail" "$unres" "$files" "$secs" "$note"
  total_pass=$((total_pass + pass)); total_fail=$((total_fail + fail)); total_unres=$((total_unres + unres))
done
incomplete=0
if [ -s "$tmp/no_sum" ]; then
  incomplete=1
  cp "$tmp/no_sum" "$archive/_log/no_sum.txt"
  echo "== archive-pass: INCOMPLETE: $(wc -l < "$tmp/no_sum") expected test directories wrote no testrun.sum (_log/no_sum.txt):"
  sed 's/^/      /' "$tmp/no_sum"
fi
if [ -s "$tmp/extra_sum" ]; then
  echo "== archive-pass: WARNING: $(wc -l < "$tmp/extra_sum") directories outside the groups wrote a testrun.sum:"
  sed 's/^/      /' "$tmp/extra_sum"
fi
# make exits non-zero whenever a test fails (runtest --status); flag it only
# when it comes with no results at all
note=
[ "$make_status" -eq 0 ] || [ $((total_pass + total_fail + total_unres)) -gt 0 ] || note="  (make exit $make_status: see _log/checkparallel.log)"
echo "== archive-pass done: pass $total_pass fail $total_fail unresolved $total_unres  archived $n_files files  make ${make_secs}s  $(( $(date +%s) - pass_t0 ))s wall  $(load_now)  $(date '+%F %T')$note"
git clean -fdXq . || die "git clean failed in $suite"
[ "$incomplete" = 0 ] || exit 4
exit 0
