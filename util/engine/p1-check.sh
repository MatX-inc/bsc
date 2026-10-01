#!/usr/bin/env bash
# P1 gate for the orchestration engine (doc/engine-first-plan.md, section 3,
# P1 exit criteria): the Bluespec libraries built by `bsc-engine libraries`
# with N workers and with one worker must install byte-identical files; a
# second run must do nothing; a shadowing or doubly-defined package must
# change the plan or fail loudly, never race; an edit to one source must
# recompile only what its content changes. Also reports how the engine's
# per-package outputs compare with a make-built installation (Base1 and
# Base2, which make compiles per file, must be identical; the Base3
# directories, which make compiles inside one `bsc -u` process, differ in
# typeclass-dictionary names: FACT 24 in the gates record).
#
# Run from the repository root with the compiler built (cabal build all) and
# a make-built installation to compare with:
#   ORACLE=/path/to/inst   (default: ./inst; needs bin/bsc, bin/bo2bloogle,
#                           bin/tconcheck and lib/Libraries)
#   OUT=results dir        (default: p1-results/<timestamp>)
# The engine is bound to the oracle's bsc so that the comparison is about
# orchestration, not compiler identity; bscdeps comes from the cabal build.
# The shared build directory build/bsvlib is used (the .bo files embed its
# path) and restored from a snapshot at the end.
set -uo pipefail
TOP=$(pwd)
ORACLE="${ORACLE:-$TOP/inst}"
OUT="${OUT:-$TOP/p1-results/$(date +%Y%m%d-%H%M%S)}"
mkdir -p "$OUT"
die() { echo "p1-check: $*" >&2; exit 2; }
ENGINE=$(cabal list-bin bsc-engine) || die "cabal list-bin bsc-engine failed"
BSCDEPS=$(cabal list-bin bscdeps) || die "cabal list-bin bscdeps failed"
for t in bsc bo2bloogle tconcheck; do [ -x "$ORACLE/bin/$t" ] || die "oracle tool missing: $ORACLE/bin/$t"; done
[ -d "$ORACLE/lib/Libraries" ] || die "oracle Libraries missing under $ORACLE/lib/Libraries"
BUILDDIR="$TOP/build/bsvlib"
COMMON=(--bsc "$ORACLE/bin/bsc" --bscdeps "$BSCDEPS" --bo2bloogle "$ORACLE/bin/bo2bloogle" --tconcheck "$ORACLE/bin/tconcheck")
fail=0
ok() { echo "PASS  $*"; }
bad() { echo "FAIL  $*"; fail=1; }

# snapshot the shared build directory
rm -rf "$OUT/snap"; mkdir -p "$OUT/snap"
[ -d "$BUILDDIR" ] && cp -a "$BUILDDIR" "$OUT/snap/bsvlib"
restore() { rm -rf "$BUILDDIR"; [ -d "$OUT/snap/bsvlib" ] && cp -a "$OUT/snap/bsvlib" "$BUILDDIR"; }
trap restore EXIT

run() { # name, then engine args
  local name=$1; shift
  local t0=$(date +%s)
  "$ENGINE" libraries "${COMMON[@]}" "$@" -V install > "$OUT/$name.log" 2>&1
  local rc=$?
  echo "$name: exit $rc, $(( $(date +%s) - t0 )) s, $(grep -c -E '^cd .*; .*/bsc ' "$OUT/$name.log") compiles"
  return $rc
}
compare() { # dirA dirB label
  local same=0 diff=0 only=0
  for f in "$1"/*; do b=$(basename "$f"); if [ -f "$2/$b" ]; then if cmp -s "$f" "$2/$b"; then same=$((same+1)); else diff=$((diff+1)); echo "$b" >> "$OUT/$3.diff"; fi; else only=$((only+1)); fi; done
  echo "$3: identical $same, differ $diff, only-in-first $only ($(ls "$2" | wc -l) files in second)"
  [ "$diff" -eq 0 ] && [ "$only" -eq 0 ]
}

echo "== 1. clean build, N workers"
rm -rf "$BUILDDIR" "$OUT/instN" "$OUT/shakeN"
run buildN --prefix "$OUT/instN" --shake-dir "$OUT/shakeN" || bad "N-worker build failed"
echo "== 2. clean build, one worker"
rm -rf "$BUILDDIR" "$OUT/inst1" "$OUT/shake1"
run build1 -j 1 --prefix "$OUT/inst1" --shake-dir "$OUT/shake1" || bad "1-worker build failed"
echo "== 3. N-worker and 1-worker installations"
if compare "$OUT/instN/lib/Libraries" "$OUT/inst1/lib/Libraries" "N-vs-1"; then ok "N-worker and 1-worker installations identical"; else bad "N-worker and 1-worker installations differ"; fi
cmp -s "$OUT/instN/lib/bloogle/bluespec.txt" "$OUT/inst1/lib/bloogle/bluespec.txt" && ok "bloogle identical" || bad "bloogle differs"
echo "== 4. against the make-built oracle (informational for Base3: -u batching, FACT 24)"
compare "$ORACLE/lib/Libraries" "$OUT/inst1/lib/Libraries" "oracle-vs-engine" > "$OUT/oracle.txt"; cat "$OUT/oracle.txt"
b12=0; b3=0
if [ -f "$OUT/oracle-vs-engine.diff" ]; then
  while read -r f; do p=${f%.*}; if ls src/Libraries/Base1/"$p".bs* src/Libraries/Base2/"$p".bs* >/dev/null 2>&1; then b12=$((b12+1)); else b3=$((b3+1)); fi; done < "$OUT/oracle-vs-engine.diff"
fi
[ "$b12" -eq 0 ] && ok "Base1/Base2 outputs identical to make's per-file compiles" || bad "$b12 Base1/Base2 outputs differ from make"
echo "note  $b3 Base3 outputs differ from make's -u batch compile (expected until dictionary names are definition-local (plan, P1 record))"
echo "== 5. second run does nothing"
run noop -j 1 --prefix "$OUT/inst1" --shake-dir "$OUT/shake1" || bad "no-op run failed"
[ "$(grep -c -E '^cd .*; .*/bsc(deps)? ' "$OUT/noop.log")" -eq 0 ] && ok "no compiler or discovery invocations on an unchanged tree" || bad "unchanged tree ran the compiler"
echo "== 6. a package defined in two directories is refused"
cp src/Libraries/Base3-Misc/HList.bsv src/Libraries/Base3-Contexts/HList.bsv
run twoproducers -j 1 --prefix "$OUT/inst1" --shake-dir "$OUT/shake1"; rc=$?
rm -f src/Libraries/Base3-Contexts/HList.bsv
if [ $rc -ne 0 ] && grep -q 'two library directories' "$OUT/twoproducers.log"; then ok "two producers refused with a diagnostic"; else bad "two producers not refused"; fi
run after-twoproducers -j 1 --prefix "$OUT/inst1" --shake-dir "$OUT/shake1" || bad "rebuild after removing the duplicate failed"
[ "$(grep -c -E '^cd .*; .*/bsc ' "$OUT/after-twoproducers.log")" -eq 0 ] && ok "removing the duplicate needs no recompilation" || bad "removing the duplicate recompiled"
echo "== 7. a shadowing source beside a package changes its resolution (file-major: .bsv before .bs)"
printf 'package Vector;\nendpackage\n' > src/Libraries/Base1/Vector.bsv
"$ENGINE" libraries "${COMMON[@]}" --prefix "$OUT/inst1" --shake-dir "$OUT/shake1" plan > "$OUT/shadow-plan.log" 2>&1; rc=$?
rm -f src/Libraries/Base1/Vector.bsv
if [ $rc -ne 0 ] && grep -q 'resolved to both Vector.bs and Vector.bsv' "$OUT/shadow-plan.log"; then ok "shadow detected as a resolution change and refused"; else bad "shadow not detected"; fi
echo "== 8. one source edit: content cutoff"
cp src/Libraries/Base1/Counter.bs "$OUT/Counter.bs.orig"
printf '\n-- p1-check marker\n' >> src/Libraries/Base1/Counter.bs
run edit -j 1 --prefix "$OUT/inst1" --shake-dir "$OUT/shake1" || bad "edit rebuild failed"
cp "$OUT/Counter.bs.orig" src/Libraries/Base1/Counter.bs
n=$(grep -c -E '^cd .*; .*/bsc ' "$OUT/edit.log")
[ "$n" -eq 1 ] && ok "a comment-only edit recompiled one package and nothing downstream" || bad "comment-only edit recompiled $n packages"
run revert -j 1 --prefix "$OUT/inst1" --shake-dir "$OUT/shake1" || bad "revert rebuild failed"
echo; echo "result: $([ $fail -eq 0 ] && echo PASS || echo FAIL)  (logs and installations in $OUT)"
exit $fail
