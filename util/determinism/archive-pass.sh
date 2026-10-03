#!/usr/bin/env bash
# archive-pass.sh -- run the bsc testsuite one top-level group at a time and
# archive every file each group generates.
#
# usage: archive-pass.sh <inst-dir> <archive-dir> [TEST_BSC_OPTIONS value]
#
#   <inst-dir>     a bsc installation; passed to make as TEST_RELEASE
#   <archive-dir>  where the generated files land, with their paths relative
#                  to testsuite/ preserved; each group's bsc.sum is kept as
#                  _sum/<group>.sum and its make transcript as _log/<group>.log
#   [options]      the TEST_BSC_OPTIONS value, e.g. -reverse-intern-order
#
# environment:
#   ONLY_GROUPS="a b c"   run only these top-level groups
#
# The worktree is the one this script lives in (util/determinism/ of it).
# Each group runs as 'make <group>.group' in <worktree>/testsuite with
# DO_INTERNAL_CHECKS=1 and TEST_RELEASE=<inst-dir>; the group list is the
# top-level entries of 'make groups.list' minus the aggregates ALL, LONG and
# dev.  The tree is cleaned with 'git clean -fdXq .' before the first group
# and after each group.  Archived: every git-ignored (hence generated) regular
# file except objects, archives, executables (*.cexe *.vexe *.syscexe and ELF
# binaries) and *.log.  Test failures never make this script exit non-zero
# (the gate compares bytes; test results are the other CI's job); the exit
# status reports only usage and I/O errors.
set -u

die() { printf 'archive-pass.sh: %s\n' "$*" >&2; exit 2; }
usage() { die 'usage: archive-pass.sh <inst-dir> <archive-dir> [TEST_BSC_OPTIONS value]'; }

[ $# -ge 2 ] && [ $# -le 3 ] || usage
inst=$1; archive=$2; bsc_options=${3:-}

script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P) || die 'cannot locate myself'
worktree=$(cd "$script_dir/../.." && pwd -P) || die 'cannot locate the worktree'
suite=$worktree/testsuite
[ -f "$suite/Makefile" ] && [ -f "$suite/test_list.sh" ] || die "no testsuite at '$suite'"

[ -x "$inst/bin/bsc" ] || die "no bsc installation at '$inst' (expected $inst/bin/bsc)"
inst=$(cd "$inst" && pwd -P) || die "cannot enter '$inst'"

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

# Print, NUL-separated, the generated files worth archiving: every git-ignored
# regular file except objects, archives, executables and logs.  *.sum is
# handled separately (bsc.sum -> _sum/<group>.sum).
generated_files() {
  local f magic
  git ls-files --others --ignored --exclude-standard -z | while IFS= read -r -d '' f; do
    [ -f "$f" ] && [ ! -L "$f" ] || continue
    case $f in
      *.o|*.so|*.a|*.dylib|*.log|*.sum|*.cexe|*.vexe|*.syscexe|*.exe|*.dSYM/*) continue ;;
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

# Copy the generated files of the current tree into the archive; print how many.
archive_group() {
  local list count
  list=$(mktemp "${TMPDIR:-/tmp}/archive-pass.XXXXXX") || die 'mktemp failed'
  generated_files > "$list"
  count=$(tr -cd '\0' < "$list" | wc -c)
  xargs -0 -r cp --parents -t "$archive/" < "$list"
  rm -f "$list"
  echo "$count"
}

count_results() {  # count_results <kind-regex> : how many such lines in bsc.sum
  local n
  n=$(grep -cE "^($1):" bsc.sum 2>/dev/null)
  echo "${n:-0}"
}

commit=$(git -C "$worktree" rev-parse --short HEAD 2>/dev/null || echo '?')
echo "== archive-pass: $commit  inst=$inst  TEST_BSC_OPTIONS='$bsc_options'  archive=$archive  $(date '+%F %T')"

total_pass=0; total_fail=0; total_unres=0; total_files=0
for g in $group_list; do
  t0=$(date +%s)
  DO_INTERNAL_CHECKS=1 TEST_BSC_OPTIONS="$bsc_options" \
    make TEST_RELEASE="$inst" "$g.group" > "$archive/_log/$g.log" 2>&1
  make_status=$?
  if [ -f bsc.sum ]; then
    cp bsc.sum "$archive/_sum/$g.sum"
    pass=$(count_results 'PASS'); fail=$(count_results 'FAIL')
    unres=$(count_results 'UNRESOLVED|ERROR')
  else
    pass=0; fail=0; unres=0
  fi
  n=$(archive_group)
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
echo "== archive-pass done: pass $total_pass fail $total_fail unresolved $total_unres  archived $total_files files  $(date '+%F %T')"
exit 0
