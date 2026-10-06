#!/usr/bin/env bash
# Hash one cabal build of the component packages: every object, archive and
# interface under dist-newstyle/build and every program the facade builds,
# with paths relative to the repository, so builds in different directories
# compare.  Objects, archives and programs are hashed as bytes.  An interface
# file is hashed as the text of `ghc --show-iface` minus its interface hash
# and its flags fingerprint: GHC fingerprints the flags it was given, and
# cabal passes the autogen include directory by absolute path, so the bytes of
# a .hi depend on the checkout directory while its content does not.  The
# package environment is switched off for that, or the text names the
# environment file and spells unit ids by what the environment exposes.
# Usage: util/recabal/hash-build.sh OUTFILE   (run from the repository root)
set -eu
TOP="$(cd "$(dirname "$0")/../.." && pwd)"; cd "$TOP"
OUT="${1:?output file}"
MANIFEST=util/recabal/manifest.json
programs=$(python3 -c 'import json,sys; print(" ".join(p["name"] for p in json.load(open(sys.argv[1]))["facade"]["programs"]))' "$MANIFEST")
ncpu=$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 4)
# macOS has no sha256sum; shasum prints the same "hash  path" lines.
sha256() { if command -v sha256sum >/dev/null 2>&1; then sha256sum "$@"; else shasum -a 256 "$@"; fi; }
iface_hash() {  # one interface file -> "hash  path"
  ghc -package-env - --show-iface "$1" | grep -v -e '^  interface hash:' -e '^  flags: fingerprint:' | sha256 | sed "s|  -\$|  $1|"
}
export -f sha256 iface_hash
{
  find dist-newstyle/build -type f \( -name '*.o' -o -name '*.dyn_o' -o -name '*.p_o' -o -name '*.a' \) -print0 \
    | sort -z | xargs -0 bash -c 'sha256 "$@"' _
  find dist-newstyle/build -type f \( -name '*.hi' -o -name '*.dyn_hi' -o -name '*.p_hi' \) -print0 \
    | xargs -0 -n 1 -P "$ncpu" bash -c 'iface_hash "$1"' _
  for e in $programs; do
    bin=$(cabal list-bin "bsc:exe:$e" 2>/dev/null) || continue
    [ -f "$bin" ] && sha256 "$bin"
  done
} | sed "s|  $TOP/|  |" | LC_ALL=C sort -k2 > "$OUT"
echo "$(wc -l < "$OUT") files hashed into $OUT"
