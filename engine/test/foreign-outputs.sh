#!/usr/bin/env bash
# Exercise the real engine's grouped outputs with cheap deterministic tools.
# BSC_ENGINE must name a built engine (or an executable wrapper around it).
set -euo pipefail

engine_bin=$(command -v "${BSC_ENGINE:-bsc-engine}")
[[ $engine_bin == /* ]] || engine_bin=$PWD/$engine_bin
test_root=$(mktemp -d "${TMPDIR:-/tmp}/bsc-engine-foreign-outputs.XXXXXX")
cleanup() {
    status=$?
    if [[ $status != 0 || ${KEEP_TMP:-0} == 1 ]]; then
        echo "engine foreign-output test files: $test_root" >&2
    else
        rm -rf -- "$test_root"
    fi
}
trap cleanup EXIT
mkdir -p "$test_root"/{tools,build,install,logs}
for directory in Base1 Base2 Base3-Misc Base3-Contexts Base3-Math; do
    mkdir -p "$test_root/src/Libraries/$directory"
done
for source in Base1/Producer Base1/Consumer Base3-Misc/Misc \
              Base3-Contexts/Contexts Base3-Math/Math; do
    printf '%s\n' "${source##*/}" > "$test_root/src/Libraries/$source.bsv"
done
printf 'test context definitions\n' > "$test_root/src/Libraries/Base3-Contexts/Contexts.defines"
export ENGINE_TEST_EVENTS="$test_root/events"

cat > "$test_root/tools/worker.py" <<'PY'
#!/usr/bin/env python3
import os
from pathlib import Path
import sys
import time

args = sys.argv[1:]
source = Path(args[-1])
package = source.stem
mode = "discover" if Path(sys.argv[0]).name == "bscdeps" else "compile"
fd = os.open(os.environ["ENGINE_TEST_EVENTS"], os.O_WRONLY | os.O_CREAT | os.O_APPEND, 0o600)
os.write(fd, f"{mode} {package}\n".encode())
os.close(fd)
imports = ["Producer"] if package in ["Consumer", "Math"] else []

if mode == "discover":
    print("bscdeps-format\t1")
    print(f"pkg\t{package}\tsrc\t{source}")
    for dependency in imports:
        # Resolve the source from the engine's discovery search path. In
        # particular, Math must discover Producer before any .bo exists.
        search = args[args.index("-p") + 1].split(":")
        found = next((Path(p) / f"{dependency}.bsv" for p in search
                      if (Path(p) / f"{dependency}.bsv").is_file()), None)
        if found is None:
            sys.exit(f"cannot discover source for {dependency}")
        print(f"imp\t{package}\t{dependency}")
        print(f"pkg\t{dependency}\tsrc\t{found}")
    if package == "Producer" or imports:
        print("foreign\tProducer\tfirst_foreign")
        print("foreign\tProducer\tsecond_foreign")
else:
    output = Path(args[args.index("-bdir") + 1])
    for dependency in imports:
        if not (output / f"{dependency}.bo").is_file():
            sys.exit(f"compiled {package} before its dependency {dependency}")
    (output / f"{package}.bo").write_text(f"package {package}\n")
    if package == "Producer":
        (output / "first_foreign.bdpi").write_text("first foreign metadata\n")
        time.sleep(0.02)
        (output / "second_foreign.bdpi").write_text("second foreign metadata\n")
PY
chmod +x "$test_root/tools/worker.py"
cp "$test_root/tools/worker.py" "$test_root/tools/bsc"
cp "$test_root/tools/worker.py" "$test_root/tools/bscdeps"

run_engine() {
    local label=$1
    if ! "$engine_bin" libraries --top "$test_root" \
        --prefix "$test_root/install" --builddir "$test_root/build" \
        --shake-dir "$test_root/database" --bsc "$test_root/tools/bsc" \
        --bscdeps "$test_root/tools/bscdeps" --lint -j 4 build \
        > "$test_root/logs/$label.log" 2>&1; then
        cat "$test_root/logs/$label.log" >&2
        echo "FAIL: $label" >&2
        exit 1
    fi
}
assert_counts() {
    python3 - "$test_root/events" "$1" <<'PY'
from collections import Counter
from pathlib import Path
import sys

counts = Counter(Path(sys.argv[1]).read_text().splitlines())
expected = int(sys.argv[2])
assert counts["compile Producer"] == expected, counts
for name in ["Consumer", "Misc", "Contexts", "Math"]:
    assert counts[f"compile {name}"] == 1, counts
assert sum(v for k, v in counts.items() if k.startswith("discover ")) == 5, counts
PY
    [[ -s "$test_root/build/first_foreign.bdpi" ]]
    [[ -s "$test_root/build/second_foreign.bdpi" ]]
}

run_engine clean
assert_counts 1
cp "$test_root/build/Producer.bo" "$test_root/Producer.expected"
run_engine unchanged
assert_counts 1
rm "$test_root/build/first_foreign.bdpi"
run_engine missing-one
assert_counts 2
cmp "$test_root/Producer.expected" "$test_root/build/Producer.bo"
rm "$test_root/build/first_foreign.bdpi" "$test_root/build/second_foreign.bdpi"
run_engine missing-both
assert_counts 3
cmp "$test_root/Producer.expected" "$test_root/build/Producer.bo"
rm "$test_root/build/Producer.bo" "$test_root/build/first_foreign.bdpi"
run_engine missing-package-and-foreign
assert_counts 4
cmp "$test_root/Producer.expected" "$test_root/build/Producer.bo"
echo "PASS: grouped foreign outputs, lint, concurrency and unchanged dependents"
