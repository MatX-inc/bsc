#!/usr/bin/env bash
# Optional FOREIGN_METADATA_VERIFY writes legacy metadata with GenABin.
# Optional BASELINE_BSC exercises metadata written by the old compiler itself.
set -euo pipefail

fixture_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
bsc_bin=$(command -v "${BSC:-bsc}")
[[ $bsc_bin == /* ]] || bsc_bin=$PWD/$bsc_bin
legacy_writer=
if [[ -n ${FOREIGN_METADATA_VERIFY:-} ]]; then
    legacy_writer=$(command -v "$FOREIGN_METADATA_VERIFY")
    [[ $legacy_writer == /* ]] || legacy_writer=$PWD/$legacy_writer
fi
baseline_bin=
if [[ -n ${BASELINE_BSC:-} ]]; then
    baseline_bin=$(command -v "$BASELINE_BSC")
    [[ $baseline_bin == /* ]] || baseline_bin=$PWD/$baseline_bin
fi
test_root=$(mktemp -d "${TMPDIR:-/tmp}/bsc-foreign-metadata.XXXXXX")
cleanup() {
    local status=$?
    if [[ $status != 0 || ${KEEP_TMP:-0} == 1 ]]; then
        echo "foreign metadata test files: $test_root" >&2
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
new_case() {
    mkdir -p "$test_root/$1"/{artifacts,output,sim}
    cd "$test_root/$1"
    cp "$fixture_dir/ForeignMetadata.bsv" .
    cp "$fixture_dir/foreign_metadata.c.keep" foreign_metadata.c
}
common=(-no-show-timestamps -no-show-version -bdir artifacts -vdir output
        -simdir sim -p +:artifacts)
link_and_run() {
    local label=$1 top=$2
    shift 2
    run "$label-link" "$bsc_bin" "${common[@]}" -sim -e "$top" \
        -o "$label" "$@" foreign_metadata.c
    run "$label-run" "./$label"
    printf '42\n' > expected.txt
    cmp expected.txt "$test_root/logs/$label-run.log"
}

new_case bluesim
run compile-sim "$bsc_bin" "${common[@]}" -sim -u ForeignMetadata.bsv
[[ -s artifacts/metadata_add.bdpi && ! -e artifacts/metadata_add.ba ]]
[[ -s artifacts/mkForeignMetadata.bmod && -s artifacts/mkForeignMetadata.bsched ]]
cp artifacts/metadata_add.bdpi expected.bdpi
rm ForeignMetadata.bsv artifacts/*.bo
link_and_run explicit-new mkForeignMetadata artifacts/metadata_add.bdpi
link_and_run implicit-new mkForeignMetadata
cmp expected.bdpi artifacts/metadata_add.bdpi

# A legacy fixture is optional in ordinary installed-compiler runs. The helper
# uses the unchanged old codec; BASELINE_BSC also verifies a real old producer.
if [[ -n $legacy_writer ]]; then
    run make-legacy "$legacy_writer" expected.bdpi artifacts/metadata_add.ba
fi
if [[ -n $baseline_bin ]]; then
    mkdir baseline
    cp "$fixture_dir/ForeignMetadata.bsv" baseline/
    (
        cd baseline
        run baseline-compile "$baseline_bin" -no-show-timestamps -no-show-version \
            -sim ForeignMetadata.bsv
    )
    cp baseline/metadata_add.ba artifacts/metadata_add.ba
fi
if [[ -s artifacts/metadata_add.ba ]]; then
    rm artifacts/metadata_add.bdpi
    link_and_run explicit-legacy mkForeignMetadata artifacts/metadata_add.ba
    link_and_run implicit-legacy mkForeignMetadata
    cp expected.bdpi artifacts/metadata_add.bdpi
    # A corrupt preferred new file must not silently fall back to valid legacy.
    printf 'invalid bdpi header\n' > artifacts/metadata_add.bdpi
    reject corrupt-with-legacy "$bsc_bin" "${common[@]}" -sim -e mkForeignMetadata \
        -o rejected foreign_metadata.c
    rm artifacts/metadata_add.ba
fi

rm -f artifacts/metadata_add.bdpi
reject missing "$bsc_bin" "${common[@]}" -sim -e mkForeignMetadata \
    -o rejected foreign_metadata.c
grep -q 'metadata_add' "$test_root/logs/missing.log"

printf 'invalid bdpi header\n' > artifacts/metadata_add.bdpi
reject invalid-header "$bsc_bin" "${common[@]}" -sim -e mkForeignMetadata \
    -o rejected artifacts/metadata_add.bdpi foreign_metadata.c
size=$(wc -c < expected.bdpi)
head -c "$((size - 1))" expected.bdpi > artifacts/metadata_add.bdpi
reject truncated "$bsc_bin" "${common[@]}" -sim -e mkForeignMetadata \
    -o rejected artifacts/metadata_add.bdpi foreign_metadata.c
cp expected.bdpi artifacts/metadata_add.bdpi
printf 'trailing garbage' >> artifacts/metadata_add.bdpi
reject trailing "$bsc_bin" "${common[@]}" -sim -e mkForeignMetadata \
    -o rejected artifacts/metadata_add.bdpi foreign_metadata.c

new_case verilog
run compile-verilog "$bsc_bin" "${common[@]}" -verilog -use-dpi -u ForeignMetadata.bsv
[[ -s artifacts/metadata_add.bdpi && ! -e artifacts/metadata_add.ba ]]
cp output/mkForeignMetadata.v expected.v
rm ForeignMetadata.bsv artifacts/*.bo output/mkForeignMetadata.v
run replay-verilog "$bsc_bin" "${common[@]}" -verilog -use-dpi \
    -c mkForeignMetadata artifacts/mkForeignMetadata.bmod
cmp expected.v output/mkForeignMetadata.v

# Modules and foreign functions have independent artifact namespaces, even
# when the foreign link name equals the synthesized module name.
new_case same-basename
rm ForeignMetadata.bsv
cp "$fixture_dir/SameName.bsv" .
cp "$fixture_dir/same_name.c.keep" foreign_metadata.c
run compile-same-basename "$bsc_bin" "${common[@]}" -sim -u SameName.bsv
[[ -s artifacts/mkSameName.bmod && -s artifacts/mkSameName.bsched ]]
[[ -s artifacts/mkSameName.bdpi && ! -e artifacts/mkSameName.ba ]]
rm SameName.bsv artifacts/*.bo
link_and_run same-name-both mkSameName artifacts/mkSameName.bmod artifacts/mkSameName.bdpi
link_and_run same-name-reversed mkSameName artifacts/mkSameName.bdpi artifacts/mkSameName.bmod
link_and_run same-name-foreign mkSameName artifacts/mkSameName.bdpi
link_and_run same-name-module mkSameName artifacts/mkSameName.bmod
link_and_run same-name-implicit mkSameName

echo "foreign metadata checks passed"
