#!/usr/bin/env bash
# Run with BSC=/path/to/candidate/bsc bash test.sh.
# Optional BASELINE_BSC compares generated Verilog with the pinned old compiler.
# Optional MODULE_PAIR_VERIFY points to VerifyModuleSplit.hs compiled with bsc-core.
set -euo pipefail

fixture_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
bsc_bin=$(command -v "${BSC:-bsc}")
[[ $bsc_bin == /* ]] || bsc_bin=$PWD/$bsc_bin
baseline_bin=
if [[ -n ${BASELINE_BSC:-} ]]; then
    baseline_bin=$(command -v "$BASELINE_BSC")
    [[ $baseline_bin == /* ]] || baseline_bin=$PWD/$baseline_bin
fi
verify_bin=
if [[ -n ${MODULE_PAIR_VERIFY:-} ]]; then
    verify_bin=$(command -v "$MODULE_PAIR_VERIFY")
    [[ $verify_bin == /* ]] || verify_bin=$PWD/$verify_bin
fi
test_root=$(mktemp -d "${TMPDIR:-/tmp}/bsc-module-split.XXXXXX")
cleanup() {
    status=$?
    if [[ $status != 0 || ${KEEP_TMP:-0} == 1 ]]; then
        echo "module split test files: $test_root" >&2
    else
        rm -rf -- "$test_root"
    fi
}
trap cleanup EXIT
mkdir -p "$test_root/logs"

run() {
    local label=$1
    shift
    if ! "$@" >"$test_root/logs/$label.log" 2>&1; then
        cat "$test_root/logs/$label.log" >&2
        echo "FAIL: $label" >&2
        exit 1
    fi
}
reject() {
    local label=$1
    shift
    if "$@" >"$test_root/logs/$label.log" 2>&1; then
        cat "$test_root/logs/$label.log" >&2
        echo "FAIL: $label unexpectedly succeeded" >&2
        exit 1
    fi
}
assert_pair() {
    local module=$1
    [[ -s artifacts/$module.bmod && -s artifacts/$module.bsched ]]
    [[ ! -e artifacts/$module.ba ]]
}
verify_pair() {
    if [[ -n $verify_bin ]]; then
        local scenario=$1 module=$2
        shift 2
        run "verify-$scenario-$module" "$verify_bin" pair "artifacts/$module.bmod" "$@"
    fi
}
assert_different() {
    if cmp -s "$1" "$2"; then
        echo "FAIL: expected different outputs: $1 and $2" >&2
        exit 1
    fi
}
new_case() {
    mkdir -p "$test_root/$1"/{artifacts,output,sim}
    cd "$test_root/$1"
}
common=(-no-show-timestamps -no-show-version -bdir artifacts -vdir output
        -simdir sim -p +:artifacts)
modules=(mkSplitCounter sysSplitModule)

new_case direct
cp "$fixture_dir/SplitModule.bsv" .
run direct "$bsc_bin" "${common[@]}" -verilog -u -g sysSplitModule SplitModule.bsv
for module in "${modules[@]}"; do
    assert_pair "$module"
    verify_pair direct "$module" -verilog
done
mv output expected
mkdir output
for suffix in bmod bsched; do
    for module in "${modules[@]}"; do
        run "codegen-$module-$suffix" "$bsc_bin" "${common[@]}" -verilog \
            -c "$module" "artifacts/$module.$suffix"
        cmp "expected/$module.v" "output/$module.v"
        rm "output/$module.v"
    done
done

# A schedule can be regenerated from .bmod alone, without source or .bo files.
new_case schedule-only
cp "$test_root/direct/artifacts/"*.bmod artifacts/
for module in "${modules[@]}"; do
    run "schedule-$module" "$bsc_bin" -verilog -schedule "artifacts/$module.bmod"
    assert_pair "$module"
    run "scheduled-codegen-$module" "$bsc_bin" "${common[@]}" -verilog \
        -c "$module" "artifacts/$module.bsched"
    cmp "$test_root/direct/expected/$module.v" "output/$module.v"
done

# -u must repair either missing half, even if the package .bo is current.
cd "$test_root/direct"
for suffix in bsched bmod; do
    rm "artifacts/mkSplitCounter.$suffix"
    run "repair-$suffix" "$bsc_bin" "${common[@]}" -verilog -u -g sysSplitModule SplitModule.bsv
    assert_pair mkSplitCounter
done

# Equal module names are insufficient: schedules identify the exact .bmod.
new_case variant
cp "$fixture_dir/SplitModule.bsv" .
run variant "$bsc_bin" "${common[@]}" -verilog -elab-only -D SPLIT_VARIANT \
    -g sysSplitModule SplitModule.bsv
new_case damaged
cp "$test_root/direct/artifacts/mkSplitCounter.bmod" artifacts/
cp "$test_root/variant/artifacts/mkSplitCounter.bsched" artifacts/
reject mismatched-pair "$bsc_bin" "${common[@]}" -verilog -c mkSplitCounter \
    artifacts/mkSplitCounter.bmod
for suffix in bmod bsched; do
    cp "$test_root/direct/artifacts/mkSplitCounter."{bmod,bsched} artifacts/
    rm "artifacts/mkSplitCounter.$suffix"
    other=bmod
    [[ $suffix == bmod ]] && other=bsched
    reject "missing-$suffix" "$bsc_bin" "${common[@]}" -verilog -c mkSplitCounter \
        "artifacts/mkSplitCounter.$other"
    cp "$test_root/direct/artifacts/mkSplitCounter."{bmod,bsched} artifacts/
    size=$(wc -c <"artifacts/mkSplitCounter.$suffix")
    head -c "$((size - 1))" "$test_root/direct/artifacts/mkSplitCounter.$suffix" \
        >"artifacts/mkSplitCounter.$suffix"
    reject "truncated-$suffix" "$bsc_bin" "${common[@]}" -verilog -c mkSplitCounter \
        "artifacts/mkSplitCounter.$other"
done

new_case bluesim
cp "$fixture_dir/SplitModule.bsv" .
run bluesim-elaborate "$bsc_bin" "${common[@]}" -sim -elab-only -g sysSplitModule SplitModule.bsv
for module in "${modules[@]}"; do
    assert_pair "$module"
    verify_pair bluesim "$module" -sim
done
for suffix in bmod bsched; do
    rm -f sim/*
    run "bluesim-codegen-$suffix" "$bsc_bin" "${common[@]}" -sim -c sysSplitModule \
        "artifacts/sysSplitModule.$suffix"
    compgen -G 'sim/*.cxx' >/dev/null
done

new_case schedule-error
cp "$fixture_dir/SplitScheduleError.bsv" .
reject schedule-error "$bsc_bin" "${common[@]}" -verilog -g sysSplitScheduleError SplitScheduleError.bsv
assert_pair sysSplitScheduleError
reject schedule-error-codegen "$bsc_bin" "${common[@]}" -verilog -c sysSplitScheduleError \
    artifacts/sysSplitScheduleError.bsched
reject schedule-error-repeat "$bsc_bin" -verilog -schedule artifacts/sysSplitScheduleError.bmod
assert_pair sysSplitScheduleError

# This fixture exercises added monitors, condition wires, guarded methods,
# noinline lifting, and undetermined-value cleanup in the replay path.
new_case features
cp "$fixture_dir/SplitScheduleFeatures.bsv" .
run features "$bsc_bin" "${common[@]}" -verilog -u -g sysSplitScheduleFeatures SplitScheduleFeatures.bsv
mv output expected
mkdir output
for file in artifacts/*.bmod; do
    module=${file##*/}
    module=${module%.bmod}
    assert_pair "$module"
    verify_pair features "$module" -verilog
    run "features-codegen-$module" "$bsc_bin" "${common[@]}" -verilog -c "$module" "$file"
    cmp "expected/$module.v" "output/$module.v"
done

# The conflict-free RWire template must also survive a fresh scheduling
# process with no evaluator inputs present.
new_case features-schedule-only
cp "$test_root/features/artifacts/"*.bmod artifacts/
for file in artifacts/*.bmod; do
    module=${file##*/}
    module=${module%.bmod}
    run "features-reschedule-$module" "$bsc_bin" -verilog -schedule "$file"
    run "features-rescheduled-codegen-$module" "$bsc_bin" "${common[@]}" -verilog \
        -c "$module" "$file"
    cmp "$test_root/features/expected/$module.v" "output/$module.v"
done

new_case starved
cp "$fixture_dir/SplitStarved.bsv" .
run starved "$bsc_bin" "${common[@]}" -verilog -remove-starved-rules \
    -g sysSplitStarved SplitStarved.bsv
assert_pair sysSplitStarved
verify_pair starved sysSplitStarved -verilog -remove-starved-rules
mv output expected
mkdir output
run starved-codegen "$bsc_bin" "${common[@]}" -verilog -c sysSplitStarved \
    artifacts/sysSplitStarved.bsched
cmp expected/sysSplitStarved.v output/sysSplitStarved.v

# Conflict-free diagnostic strings embed source positions. Remapping them
# must be identical before serialization and after reconstructing the pair.
new_case features-remapped
cp "$fixture_dir/SplitScheduleFeatures.bsv" .
run features-remapped "$bsc_bin" "${common[@]}" -verilog -u \
    -remap-path-prefix "$PWD=/module-split/source" \
    -g sysSplitScheduleFeatures "$PWD/SplitScheduleFeatures.bsv"
mv output expected
mkdir output
for file in artifacts/*.bmod; do
    module=${file##*/}
    module=${module%.bmod}
    assert_pair "$module"
    verify_pair features-remapped "$module" -verilog \
        -remap-path-prefix "$PWD=/module-split/source"
    run "features-remapped-codegen-$module" "$bsc_bin" "${common[@]}" -verilog \
        -remap-path-prefix "$PWD=/module-split/source" -c "$module" "$file"
    cmp "expected/$module.v" "output/$module.v"
done

# Container iteration in use conditions must not leak string-intern order
# into the generated conflict-free checks, in either process or direction.
new_case conditions
cp "$fixture_dir/SplitUseConditions.bsv" .
run conditions "$bsc_bin" "${common[@]}" -verilog -g sysSplitUseConditions SplitUseConditions.bsv
assert_pair sysSplitUseConditions
verify_pair conditions sysSplitUseConditions -verilog
cp output/sysSplitUseConditions.v expected.v
for mode in normal reverse; do
    mode_flags=()
    [[ $mode == reverse ]] && mode_flags=(-reverse-intern-order)
    run "conditions-replay-$mode" "$bsc_bin" "${common[@]}" "${mode_flags[@]}" \
        -verilog -c sysSplitUseConditions artifacts/sysSplitUseConditions.bsched
    cmp expected.v output/sysSplitUseConditions.v
done
run conditions-reverse "$bsc_bin" "${common[@]}" -reverse-intern-order \
    -verilog -g sysSplitUseConditions SplitUseConditions.bsv
cmp expected.v output/sysSplitUseConditions.v
run conditions-reverse-replay "$bsc_bin" "${common[@]}" -verilog \
    -c sysSplitUseConditions artifacts/sysSplitUseConditions.bmod
cmp expected.v output/sysSplitUseConditions.v
run conditions-reverse-schedule "$bsc_bin" -verilog -reverse-intern-order \
    -schedule artifacts/sysSplitUseConditions.bmod
run conditions-rescheduled-replay "$bsc_bin" "${common[@]}" -verilog \
    -c sysSplitUseConditions artifacts/sysSplitUseConditions.bmod
cmp expected.v output/sysSplitUseConditions.v

# Resolve undefined values when constructing the scheduled module. Different
# choices leave .bmod unchanged but change the concrete IR saved in .bsched.
# Later code generation must preserve that IR under default or opposite flags.
new_case backend-options
cp "$fixture_dir/SplitInvocationOptions.bsv" .
for value in 0 1; do
    run "backend-options-direct-$value" "$bsc_bin" "${common[@]}" -verilog \
        -unspecified-to "$value" -g sysSplitInvocationOptions SplitInvocationOptions.bsv
    assert_pair sysSplitInvocationOptions
    cp output/sysSplitInvocationOptions.v "expected-$value.v"
    if [[ $value == 0 ]]; then
        cp artifacts/sysSplitInvocationOptions.bmod original.bmod
        cp artifacts/sysSplitInvocationOptions.bsched original.bsched
    else
        cmp original.bmod artifacts/sysSplitInvocationOptions.bmod
        assert_different original.bsched artifacts/sysSplitInvocationOptions.bsched
    fi
done
assert_different expected-0.v expected-1.v
# A source pragma determines the saved concrete IR, without retaining argv.
# Equal-length pragmas keep source positions and synthesis properties equal.
for value in 0 1; do
    sed "s/synthesize \*/synthesize, options = \"-unspecified-to $value\" */" \
        "$fixture_dir/SplitInvocationOptions.bsv" > SplitInvocationOptions.bsv
    run "backend-options-pragma-$value" "$bsc_bin" "${common[@]}" -verilog \
        -unspecified-to "$((1 - value))" -g sysSplitInvocationOptions SplitInvocationOptions.bsv
    cmp "expected-$value.v" output/sysSplitInvocationOptions.v
    cp artifacts/sysSplitInvocationOptions.bsched "pragma-$value.bsched"
    if [[ $value == 0 ]]; then
        cp artifacts/sysSplitInvocationOptions.bmod pragma.bmod
    else
        cmp pragma.bmod artifacts/sysSplitInvocationOptions.bmod
        assert_different pragma-0.bsched pragma-1.bsched
    fi
done
rm SplitInvocationOptions.bsv artifacts/*.bo
for value in 0 1; do
    cp "pragma-$value.bsched" artifacts/sysSplitInvocationOptions.bsched
    for invocation in default opposite; do
        replay_flags=()
        [[ $invocation == opposite ]] && replay_flags=(-unspecified-to "$((1 - value))")
        run "backend-options-replay-$value-$invocation" "$bsc_bin" "${common[@]}" -verilog \
            "${replay_flags[@]}" -c sysSplitInvocationOptions \
            artifacts/sysSplitInvocationOptions.bsched
        cmp "expected-$value.v" output/sysSplitInvocationOptions.v
        verify_pair "backend-options-$value-$invocation" sysSplitInvocationOptions \
            -verilog "${replay_flags[@]}"
    done
    cmp pragma.bmod artifacts/sysSplitInvocationOptions.bmod
    cmp "pragma-$value.bsched" artifacts/sysSplitInvocationOptions.bsched
done
# Explicitly rescheduling .bmod applies the new invocation's resolution choice.
for value in 0 1; do
    run "backend-options-reschedule-$value" "$bsc_bin" -verilog \
        -unspecified-to "$value" -schedule artifacts/sysSplitInvocationOptions.bmod
    run "backend-options-rescheduled-replay-$value" "$bsc_bin" "${common[@]}" -verilog \
        -unspecified-to "$((1 - value))" -c sysSplitInvocationOptions \
        artifacts/sysSplitInvocationOptions.bsched
    cmp "expected-$value.v" output/sysSplitInvocationOptions.v
    cmp pragma.bmod artifacts/sysSplitInvocationOptions.bmod
done

# Scheduler options are also supplied afresh. A .bmod originally compiled
# while pruning starved rules must support scheduling with that option off,
# then on again, without recompilation or rewriting the .bmod.
new_case scheduler-options
cp "$fixture_dir/SplitStarved.bsv" .
for choice in keep remove; do
    schedule_flags=()
    [[ $choice == remove ]] && schedule_flags=(-remove-starved-rules)
    run "scheduler-options-direct-$choice" "$bsc_bin" "${common[@]}" -verilog \
        -keep-fires "${schedule_flags[@]}" -g sysSplitStarved SplitStarved.bsv
    assert_pair sysSplitStarved
    cp artifacts/sysSplitStarved.bsched "expected-$choice.bsched"
    cp output/sysSplitStarved.v "expected-$choice.v"
    if [[ $choice == keep ]]; then
        cp artifacts/sysSplitStarved.bmod original.bmod
    else
        cmp original.bmod artifacts/sysSplitStarved.bmod
    fi
done
assert_different expected-keep.bsched expected-remove.bsched
assert_different expected-keep.v expected-remove.v
rm SplitStarved.bsv artifacts/*.bo
for choice in keep remove; do
    schedule_flags=()
    [[ $choice == remove ]] && schedule_flags=(-remove-starved-rules)
    run "scheduler-options-reschedule-$choice" "$bsc_bin" -verilog \
        "${schedule_flags[@]}" -schedule artifacts/sysSplitStarved.bmod
    cmp original.bmod artifacts/sysSplitStarved.bmod
    if [[ -n $verify_bin ]]; then
        # Interning order may reorder CUse diagnostic explanations; compare
        # the full schedule after normalizing only those explanation lists.
        run "scheduler-options-verify-$choice" "$verify_bin" compare-schedules \
            "expected-$choice.bsched" artifacts/sysSplitStarved.bsched
    fi
    run "scheduler-options-replay-$choice" "$bsc_bin" "${common[@]}" -verilog \
        -keep-fires -c sysSplitStarved artifacts/sysSplitStarved.bsched
    cmp "expected-$choice.v" output/sysSplitStarved.v
done

# Method-before-rule decisions are part of the saved schedule. A codegen
# invocation's scheduler preferences must not change those saved relations.
new_case method-order
cp "$fixture_dir/SplitMethodOrder.bsv" .
run method-order-direct "$bsc_bin" "${common[@]}" -verilog \
    -no-relax-method-earliness -g mkSplitMethodOrder SplitMethodOrder.bsv
assert_pair mkSplitMethodOrder
cp output/mkSplitMethodOrder.v expected.v
cp artifacts/mkSplitMethodOrder.bmod original.bmod
cp artifacts/mkSplitMethodOrder.bsched original.bsched
rm SplitMethodOrder.bsv artifacts/*.bo
for option in -relax-method-earliness -no-relax-method-earliness; do
    run "method-order-replay$option" "$bsc_bin" "${common[@]}" -verilog \
        "$option" -c mkSplitMethodOrder artifacts/mkSplitMethodOrder.bsched
    cmp expected.v output/mkSplitMethodOrder.v
done
if [[ -n $verify_bin ]]; then
    run method-order-verify "$verify_bin" method-order artifacts/mkSplitMethodOrder.bmod \
        -verilog -no-relax-method-earliness
fi
cmp original.bmod artifacts/mkSplitMethodOrder.bmod
cmp original.bsched artifacts/mkSplitMethodOrder.bsched

if [[ -n $baseline_bin ]]; then
    # Use the same paths and flags to avoid changing source-location metadata.
    for scenario in direct features starved; do
        cd "$test_root/$scenario"
        verify_flags=(-verilog)
        case $scenario in
            direct) compile_args=(-u -g sysSplitModule SplitModule.bsv) ;;
            features) compile_args=(-u -g sysSplitScheduleFeatures SplitScheduleFeatures.bsv) ;;
            starved)
                compile_args=(-remove-starved-rules -g sysSplitStarved SplitStarved.bsv)
                verify_flags+=(-remove-starved-rules)
                ;;
        esac
        mv artifacts candidate-artifacts
        mkdir artifacts
        rm -f output/*
        run "baseline-$scenario" "$baseline_bin" "${common[@]}" -verilog "${compile_args[@]}"
        mv artifacts baseline-artifacts
        mv candidate-artifacts artifacts
        for file in artifacts/*.bmod; do
            module=${file##*/}
            module=${module%.bmod}
            cmp "expected/$module.v" "output/$module.v"
            if [[ -n $verify_bin ]]; then
                run "baseline-payload-$module" "$verify_bin" compare \
                    "artifacts/$module.bmod" "baseline-artifacts/$module.ba" \
                    "${verify_flags[@]}"
            fi
        done
    done
fi
echo 'PASS: module artifact pair, invocation options, replay, rescheduling, integrity, and backend checks'
