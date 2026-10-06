#!/usr/bin/env bash
# The determinism ladder on one machine (doc/recabalization-brief.md, 3.7,
# G1''): clean builds with the project's parallel settings, oversubscribed,
# and serial must produce bit-identical objects, interfaces, archives and
# programs; then an incremental round (marker edit per component, rebuild,
# revert, rebuild) must land back on the same bytes.  Each build is hashed
# by hash-build.sh and the lists compared by ladder-verdict.sh, the same two
# halves the CI ladder (.github/workflows/matx-cabal.yml) runs across
# machines and directories.  Run from the repository root.  Results and hash
# lists go to $OUT (default: determinism-results/<timestamp>).
set -u
TOP="$(cd "$(dirname "$0")/../.." && pwd)"; cd "$TOP"
STAMP="$(date -u +%Y%m%dT%H%M%SZ)"
OUT="${OUT:-$TOP/determinism-results/$STAMP}"; mkdir -p "$OUT"
MANIFEST=util/recabal/manifest.json
die() { echo "determinism-check: $*" >&2; exit 2; }
command -v cabal >/dev/null || die "cabal not found"
ghc --numeric-version | grep -q '^9\.1[4-9]\.\|^9\.[2-9][0-9]\.\|^[1-9][0-9]\.' || die "GHC 9.14 or later required for -fobject-determinism, found $(ghc --numeric-version)"
git diff --quiet || die "working tree is dirty; commit or stash first"
[ -e cabal.project.local ] && die "cabal.project.local exists; the ladder writes its own"

snapshot() { util/recabal/hash-build.sh "$OUT/$1.sha256"; }
compare() { util/recabal/ladder-verdict.sh "$OUT/$1.sha256" "$OUT/$2.sha256"; }
clean() { rm -rf dist-newstyle/build dist-newstyle/cache dist-newstyle/packagedb dist-newstyle/tmp; }
leg_config() {  # N: cabal jobs and ghc -j both N (a later -j wins in GHC); empty = project defaults
    if [ -n "$1" ]; then printf 'jobs: %s\nprogram-options\n  ghc-options: -j%s\n' "$1" "$1" > cabal.project.local
    else rm -f cabal.project.local; fi
}
build() {  # name
    local name="$1" t0
    t0=$(date +%s)
    cabal build all > "$OUT/$name.log" 2>&1; local rc=$?
    echo "$name: exit $rc, $(( $(date +%s) - t0 )) s"
    [ $rc -eq 0 ] || { tail -30 "$OUT/$name.log"; rm -f cabal.project.local; die "$name build failed"; }
}
trap 'rm -f cabal.project.local' EXIT
fail=0
leg_config "";  clean; build A;  snapshot A
leg_config "";  clean; build B;  snapshot B;  compare A B || fail=1
leg_config 1;   clean; build C;  snapshot C;  compare A C || fail=1
leg_config 64;  clean; build D;  snapshot D;  compare A D || fail=1
# incremental: a marker edit to one root module per component, rebuild, revert, rebuild
leg_config ""
python3 - "$MANIFEST" <<'PY' > "$OUT/leaf-modules.txt"
import json, os, sys
m = json.load(open(sys.argv[1]))
roots = m.get("source_roots", ["src/comp"])
skip = set(m.get("generated_modules", [])) | {"Warmup", "BuildSystem", "BuildVersion"}
for c in m["components"]:
    for mod in c["modules"]:
        if mod in skip:
            continue
        rel = mod.replace(".", "/")
        hits = [f"{r}/{rel}{ext}" for r in roots for ext in m.get("source_extensions", [".hs", ".lhs"])
                if os.path.exists(f"{r}/{rel}{ext}")]
        if hits:
            print(hits[0]); break
PY
while read -r f; do echo "-- determinism-check marker" >> "$f"; done < "$OUT/leaf-modules.txt"
build EDIT; snapshot EDIT
git checkout -- $(cat "$OUT/leaf-modules.txt")
build REVERT; snapshot REVERT; compare A REVERT || fail=1
echo; echo "result: $([ $fail -eq 0 ] && echo PASS || echo FAIL)  (hash lists and logs in $OUT)"
exit $fail
