#!/usr/bin/env bash
# run-gate.sh -- the bsc determinism gate, end to end.
#
# usage: run-gate.sh [inst-dir] [archive-root] [compare.py options...]
#
# Runs the testsuite twice from <worktree>/testsuite with archive-pass.sh,
# once normally and once with TEST_BSC_OPTIONS=-reverse-intern-order,
# archiving the generated files under <archive-root>/normal and
# <archive-root>/reversed, then compares the two archives with compare.py
# against the allowlist.txt and ignore.txt committed next to this script.
# Exits with compare.py's status: 0 green, 1 a file differs that is neither
# allow-listed nor ignored, 2 stale allow-list entries, 3 usage or I/O error.
#
#   <inst-dir>      the bsc installation to test; defaults to $TEST_RELEASE,
#                   then <worktree>/inst
#   <archive-root>  defaults to $GATE_OUT, then <worktree>/build/determinism;
#                   a previous run's normal/, reversed/, pass-*.log and
#                   compare.* there are removed first
#   options starting with '--' are passed on to compare.py, e.g.
#   --allow-stale, --root-counts, --write-allowlist F, --jobs N
#
# environment:
#   ONLY_GROUPS="a b c"   restrict both passes to these top-level groups; the
#                         same list goes to compare.py as --scope-groups so
#                         that the other groups' allow-list entries are not
#                         reported stale
#   TEST_RELEASE, GATE_OUT   defaults for the two positional arguments (CI
#                         sets these and passes no arguments)
set -u -o pipefail

die() { printf 'run-gate.sh: %s\n' "$*" >&2; exit 3; }
usage() { die 'usage: run-gate.sh [inst-dir] [archive-root] [compare.py options...]'; }

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
rm -rf "$root/normal" "$root/reversed" "$root"/pass-*.log "$root"/compare.*

echo "== run-gate: worktree $worktree  inst $inst  archives $root  $(date '+%F %T')"
# The allow-list is a baseline for one bsc build (see the header of
# allowlist.txt); say which build this is so a mismatch is visible in the log.
echo "== run-gate: $("$inst/bin/bsc" -v 2>&1 | head -n 1)"
if [ -n "${ONLY_GROUPS:-}" ]; then
  echo "== run-gate: ONLY_GROUPS='$ONLY_GROUPS'"
  compare_opts+=(--scope-groups "$ONLY_GROUPS")
fi

"$here/archive-pass.sh" "$inst" "$root/normal" 2>&1 | tee "$root/pass-normal.log" \
  || die "archive-pass.sh (normal) failed"
"$here/archive-pass.sh" "$inst" "$root/reversed" -reverse-intern-order 2>&1 | tee "$root/pass-reversed.log" \
  || die "archive-pass.sh (reversed) failed"

echo "== run-gate: comparing  $(date '+%F %T')"
python3 "$here/compare.py" "$root/normal" "$root/reversed" \
  --allowlist "$allowlist" --ignore "$ignore" --out "$root/compare" \
  ${compare_opts[@]+"${compare_opts[@]}"}
status=$?
echo "== run-gate: done, exit $status  $(date '+%F %T')"
exit "$status"
