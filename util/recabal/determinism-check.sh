#!/usr/bin/env bash
# G1'' for the recabalization (doc/recabalization-brief.md, 3.7): three clean
# builds (two with the project's parallel settings, one serial) must produce
# bit-identical objects, interfaces and executables; then an incremental
# round (marker edit per component, rebuild, revert, rebuild) must land back
# on the same bytes. Run from the repository root. Results and hash lists go
# to $OUT (default: determinism-results/<timestamp>).
set -u
TOP="$(cd "$(dirname "$0")/../.." && pwd)"; cd "$TOP"
STAMP="$(date -u +%Y%m%dT%H%M%SZ)"
OUT="${OUT:-$TOP/determinism-results/$STAMP}"; mkdir -p "$OUT"
MANIFEST=util/recabal/manifest.json
die() { echo "determinism-check: $*" >&2; exit 2; }
command -v cabal >/dev/null || die "cabal not found"
ghc --numeric-version | grep -q '^9\.14\.' || die "GHC 9.14.x required, found $(ghc --numeric-version)"
git diff --quiet || die "working tree is dirty; commit or stash first"

exes() { for e in bsc bluetcl bsc2bsv bscdeps bsv2bsc dumpba dumpbo fstcheck fstscopes showrules vcdcheck; do cabal list-bin "bsc:exe:$e" 2>/dev/null; done; }
snapshot() {  # name
    local name="$1" list="$OUT/$1.sha256"
    { find dist-newstyle/build -type f \( -name '*.o' -o -name '*.hi' -o -name '*.dyn_o' -o -name '*.dyn_hi' -o -name '*.p_o' -o -name '*.p_hi' \) -print0 | sort -z | xargs -0 sha256sum
      exes | sort | xargs -r sha256sum; } | sed "s|$TOP/||" > "$list"
    echo "$name: $(wc -l < "$list") files hashed"
}
build() {  # name args...
    local name="$1"; shift
    local t0=$(date +%s)
    cabal build all "$@" > "$OUT/$name.log" 2>&1; local rc=$?
    echo "$name: exit $rc, $(( $(date +%s) - t0 )) s"; [ $rc -eq 0 ] || { tail -30 "$OUT/$name.log"; die "$name build failed"; }
}
clean() { rm -rf dist-newstyle/build dist-newstyle/cache dist-newstyle/packagedb dist-newstyle/tmp; }
compare() {  # a b
    if diff -q "$OUT/$1.sha256" "$OUT/$2.sha256" >/dev/null; then echo "IDENTICAL: $1 vs $2"; return 0
    else echo "DIFFER: $1 vs $2"; diff "$OUT/$1.sha256" "$OUT/$2.sha256" | grep '^[<>]' | awk '{print $NF}' | sort -u | head -40 > "$OUT/diff-$1-$2.txt"; sed 's/^/   /' "$OUT/diff-$1-$2.txt"; return 1; fi
}
fail=0
clean; build A; snapshot A
clean; build B; snapshot B
compare A B || fail=1
clean; build C -j1 --ghc-options=-j1; snapshot C
compare A C || fail=1
# incremental: a marker edit to one root module per component, rebuild, revert, rebuild
clean; build A2 >/dev/null; snapshot A2; compare A A2 || fail=1
python3 - "$MANIFEST" <<'PY' > "$OUT/leaf-modules.txt"
import json,sys,os
m=json.load(open(sys.argv[1]))
comps=m['components'] if isinstance(m,dict) and 'components' in m else m
for c in (comps if isinstance(comps,list) else comps.values()):
    mods=c['modules'] if isinstance(c,dict) else c
    for mod in mods:
        if mod in ('Warmup','BuildSystem','BuildVersion'): continue
        p='src/comp/'+mod.replace('.','/')+'.hs'
        if os.path.exists(p): print(p); break
PY
while read -r f; do echo "-- determinism-check marker" >> "$f"; done < "$OUT/leaf-modules.txt"
build EDIT; snapshot EDIT
git checkout -- $(cat "$OUT/leaf-modules.txt")
build REVERT; snapshot REVERT
compare A REVERT || fail=1
echo; echo "result: $([ $fail -eq 0 ] && echo PASS || echo FAIL)  (hash lists and logs in $OUT)"
exit $fail
