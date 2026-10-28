#!/usr/bin/env bash
# Public native-capture -> checked flow -> metrics -> independent branch replay.
set -eu
if [ "$#" -ne 6 ]; then
  echo "usage: run.sh BSC BLUEHS ECO_SDK YOSYS BLUESPECDIR FRESH_OUTPUT" >&2
  exit 2
fi
bsc=$(cd "$(dirname "$1")" && pwd)/$(basename "$1")
bluehs=$(cd "$(dirname "$2")" && pwd)/$(basename "$2")
sdk=$(cd "$3" && pwd)
yosys=$(cd "$(dirname "$4")" && pwd)/$(basename "$4")
export BLUESPECDIR=$(cd "$5" && pwd)
export LD_LIBRARY_PATH="$BLUESPECDIR/SAT${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
repo=$(cd "$(dirname "$0")/../../.." && pwd)
if [ -e "$6" ] || [ -L "$6" ]; then
  echo "example output already exists; use a fresh path" >&2
  exit 2
fi
mkdir -p "$6"
work=$(cd "$6" && pwd)
cd "$work"
"$bluehs" "$sdk/scripts/package-sdk.hs" --check "$sdk/build.manifest"
# Optional reproducibility controls for this fixture, not required ECO settings.
flags=(-u -cpp -verilog -elab -no-show-timestamps -keep-fires
  -keep-inlined-boundaries -no-inline-bool -no-opt-sched
  -no-opt-aggressive-inline -no-opt-join-defs -no-opt-AndOr)
for side in old new; do
  mkdir -p "$work/$side/build" "$work/$side/capture"
  edit=()
  if [ "$side" = new ]; then edit=(-Xcpp -DECO_NEW); fi
  "$bsc" "${flags[@]}" "${edit[@]}" -g mkFanoutLoss \
    -bdir "$work/$side/build" -vdir "$work/$side/build" -info-dir "$work/$side/build" \
    -eco-capture "$work/$side/capture" "$repo/testsuite/bsc.eco/postba/FanoutLoss.bs" \
    > "$work/$side/capture.log" 2>&1
 done
"$bluehs" "-i$sdk/lib" "$sdk/scripts/configure.hs" strict "$bsc" mkFanoutLoss \
  "$work/old/capture/mkFanoutLoss.replay" "$work/new/capture/mkFanoutLoss.replay" \
  "$work" "$work" "$BLUESPECDIR" "$sdk/build.manifest" "$yosys" \
  "$work/flow" "$work/flow.config"
case "${ECO_EXAMPLE_POLICY:-standard}" in
  standard)
    capture_args=(--objective translated-normalized --refinements standard --structure ordinary)
    objective=translated-normalized ;;
  legacy-selection-fixture)
    # Existing selector negative controls require the historical ordinary-plan fixture.
    capture_args=(--vacant-state --state-view --historical-shell --inactive-helpers)
    objective=whole ;;
  *) echo "unknown example policy" >&2; exit 2 ;;
esac
"$bluehs" "-i$sdk/lib" "$sdk/scripts/captures.hs" "${capture_args[@]}" \
  --invariants defer "$work/flow.config" > "$work/flow.log" 2>&1
"$bluehs" "-i$sdk/lib" "$sdk/scripts/finish-order.hs" "$sdk/build.manifest" \
  "$work/flow" "$work/finished" > "$work/finish-order.log" 2>&1
old="$work/flow/adapted-old/mkFanoutLoss.eco-ast"
new="$work/flow/adapted-new/mkFanoutLoss.eco-ast"
"$bluehs" "-i$sdk/lib" "$sdk/scripts/measure.hs" "$old" "$new" "$work/ordinary-metrics"
branch=$(sed -n 's/^selected_branch=//p' "$work/flow/summary.txt")
case "$branch" in
  kept) context="$work/flow/kept/mkFanoutLoss.eco-ast" ;;
  ordinary) context="$new" ;;
  ordinary-rollback)
    "$bluehs" "-i$sdk/lib" "$sdk/scripts/measure.hs" "$old" "$new" "$work/selected-metrics"
    "$bluehs" "$sdk/scripts/package-sdk.hs" --check "$sdk/build.manifest"
    cat "$work/flow/summary.txt"
    exit 0 ;;
  *) echo "unknown selected branch" >&2; exit 1 ;;
esac
"$bluehs" "-i$sdk/lib" "$sdk/scripts/measure.hs" --candidate "$old" "$context" \
  "$work/flow/$branch-search/selected.ast" "$work/flow/mkFanoutLoss.v" "$work/selected-metrics"
replay_args=()
if [ "$objective" != whole ]; then replay_args=(--objective "$objective" "$old"); fi
"$bluehs" "-i$sdk/lib" "$sdk/scripts/replay.hs" "${replay_args[@]}" "$work/flow/effective.config" "$context" \
  "$work/flow/$branch-search/search.plan" "$work/replayed-branch"
cmp "$work/flow/mkFanoutLoss.v" "$work/replayed-branch/selected.v"
"$bluehs" "$sdk/scripts/package-sdk.hs" --check "$sdk/build.manifest"
cat "$work/flow/summary.txt"
