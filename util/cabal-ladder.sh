#!/usr/bin/env bash
# The determinism ladder of the cabal build (doc/cabal.md): one commit built
# serially, oversubscribed, incrementally or in another directory must give
# bit-identical objects, interfaces, archives and programs.  CI
# (.github/workflows/cabal.yml) runs `hash` on every leg and `verdict` over
# the lists; `run` is the whole ladder on one machine.
set -u
SELF="$(cd "$(dirname "$0")" && pwd)/$(basename "$0")"
TOP="$(cd "$(dirname "$SELF")/.." && pwd)"; cd "$TOP" || exit 2
LEAF=src/comp/Libs/IOUtil.hs
usage() {
  cat >&2 <<USAGE
usage: util/cabal-ladder.sh hash OUT.sha256       hash the build in dist-newstyle into OUT.sha256
       util/cabal-ladder.sh verdict REF OTHER...  compare hash lists against REF (an absent one is MISSING)
       util/cabal-ladder.sh run                   the ladder on this machine; lists and logs in \$OUT
                                                  (default determinism-results/<stamp>)
USAGE
  exit 2
}
die() { echo "cabal-ladder: $*" >&2; exit 2; }
# macOS has no sha256sum; shasum prints the same "hash  path" lines.
sha256() { if command -v sha256sum >/dev/null 2>&1; then sha256sum "$@"; else shasum -a 256 "$@"; fi; }
# An interface is hashed as the text of `ghc --show-iface` minus its interface
# hash and flags fingerprint: cabal passes the autogen include directory by
# absolute path and GHC fingerprints its flags, so the bytes of a .hi depend on
# the checkout directory while its content does not.  The package environment
# is off, or the text names the environment file and spells unit ids by it.
iface_hash() {  # one interface file -> "hash  path"
  local text
  text="$(ghc -package-env - --show-iface "$1")" || { echo "cabal-ladder: ghc --show-iface failed on $1" >&2; exit 1; }
  [ -n "$text" ] || { echo "cabal-ladder: ghc --show-iface printed nothing for $1" >&2; exit 1; }
  printf '%s\n' "$text" | grep -v -e '^  interface hash:' -e '^  flags: fingerprint:' | sha256 | sed "s|  -\$|  $1|"
}
export -f sha256 iface_hash

# Every object, archive and interface under dist-newstyle/build and every
# program bsc.cabal declares, as bytes except interfaces, with paths relative
# to the repository so builds in different directories compare.
hash_build() {
  LIST="$1"
  # A partial list would compare like a complete one.
  trap '[ $? -eq 0 ] || rm -f "$LIST"' EXIT
  local ncpu programs e bin empty_digest
  ncpu=$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 4)
  programs=$(awk '/^executable[ \t]/ {print $2}' bsc.cabal)
  [ -n "$programs" ] || die "no executable stanzas in bsc.cabal"
  {
    find dist-newstyle/build -type f \( -name '*.o' -o -name '*.dyn_o' -o -name '*.p_o' -o -name '*.a' \) -print0 \
      | sort -z | xargs -0 bash -eo pipefail -c 'sha256 "$@"' _
    find dist-newstyle/build -type f \( -name '*.hi' -o -name '*.dyn_hi' -o -name '*.p_hi' \) -print0 \
      | xargs -0 -n 1 -P "$ncpu" bash -eo pipefail -c 'iface_hash "$1"' _
    for e in $programs; do
      bin=$(cabal list-bin "bsc:exe:$e" 2>/dev/null) || continue
      [ -f "$bin" ] || continue
      sha256 "$bin"
    done
  } | sed "s|  $TOP/|  |" | LC_ALL=C sort -k2 > "$LIST"
  # A list without interfaces, or with the digest of empty text, means the
  # interface hashing silently produced nothing; it must not compare IDENTICAL.
  grep -q '\.hi$' "$LIST" || die "no .hi files hashed into $LIST"
  empty_digest="$(sha256 < /dev/null | cut -d' ' -f1)"
  grep -q "^$empty_digest " "$LIST" && die "$LIST contains the digest of empty text"
  echo "$(wc -l < "$LIST") files hashed into $LIST"
}

# Pass means every list is identical to the reference.  Differences are
# reported by category (program, object, interface, archive) so a failure
# says what moved.
verdict() {
  local ref="$1" fail=0 other name diffs n cat sel c; shift
  [ -r "$ref" ] && [ -s "$ref" ] || { echo "verdict: reference $ref is missing, unreadable or empty"; exit 2; }
  for other in "$@"; do
    name="$(basename "$other" .sha256)"
    if [ ! -r "$other" ] || [ ! -s "$other" ]; then echo "MISSING: $name (no hash list)"; fail=1; continue; fi
    if diff -q "$ref" "$other" >/dev/null; then echo "IDENTICAL: $(basename "$ref" .sha256) vs $name ($(wc -l < "$other") files)"; continue; fi
    fail=1
    diffs=$(diff "$ref" "$other" | grep '^[<>]' | awk '{print $NF}' | sort -u)
    n=$(printf '%s\n' "$diffs" | sed '/^$/d' | wc -l)
    echo "DIFFER: $(basename "$ref" .sha256) vs $name: $n file(s)"
    for cat in program object interface archive; do
      case "$cat" in
        program) sel=$(printf '%s\n' "$diffs" | grep -Ev '\.(o|hi|dyn_o|dyn_hi|p_o|p_hi|a)$' || true);;
        object) sel=$(printf '%s\n' "$diffs" | grep -E '\.(o|dyn_o|p_o)$' || true);;
        interface) sel=$(printf '%s\n' "$diffs" | grep -E '\.(hi|dyn_hi|p_hi)$' || true);;
        archive) sel=$(printf '%s\n' "$diffs" | grep '\.a$' || true);;
      esac
      c=$(printf '%s\n' "$sel" | sed '/^$/d' | wc -l)
      [ "$c" -gt 0 ] && { echo "   $cat: $c"; printf '%s\n' "$sel" | sed '/^$/d' | head -8 | sed 's/^/      /'; }
    done
  done
  echo "result: $([ $fail -eq 0 ] && echo PASS || echo FAIL)"
  exit $fail
}

# Clean builds with the project's settings (A, B), serial (C) and
# oversubscribed (D), then an incremental round (marker edit of one module
# module, rebuild, revert, rebuild) that must land back on A's bytes.
run_ladder() {
  OUT="${OUT:-$TOP/determinism-results/$(date -u +%Y%m%dT%H%M%SZ)}"; mkdir -p "$OUT"
  command -v cabal >/dev/null || die "cabal not found"
  ghc --numeric-version | grep -Eq '^(9\.1[4-9]|9\.[2-9][0-9]|[1-9][0-9])\.' \
    || die "GHC 9.14 or later required for -fobject-determinism, found $(ghc --numeric-version)"
  git diff --quiet || die "working tree is dirty; commit or stash first"
  [ -e cabal.project.local ] && die "cabal.project.local exists; the ladder writes its own"
  [ -f "$LEAF" ] || die "$LEAF not found"
  snapshot() { "$SELF" hash "$OUT/$1.sha256" || die "$1: hashing the build failed"; }
  compare() { "$SELF" verdict "$OUT/$1.sha256" "$OUT/$2.sha256" || fail=1; }
  clean() { rm -rf dist-newstyle/build dist-newstyle/cache dist-newstyle/packagedb dist-newstyle/tmp; }
  leg_config() {  # N: cabal jobs and ghc -j both N (a later -j wins in GHC); empty = project defaults
    if [ -n "$1" ]; then printf 'jobs: %s\nprogram-options\n  ghc-options: -j%s\n' "$1" "$1" > cabal.project.local
    else rm -f cabal.project.local; fi
  }
  build() {  # name
    local name="$1" t0 rc=0
    t0=$(date +%s)
    cabal build all > "$OUT/$name.log" 2>&1 || rc=$?
    echo "$name: exit $rc, $(( $(date +%s) - t0 )) s"
    [ $rc -eq 0 ] || { tail -30 "$OUT/$name.log"; die "$name build failed"; }
  }
  trap 'rm -f cabal.project.local; git checkout -- "$LEAF"' EXIT
  local fail=0
  leg_config "";  clean; build A;  snapshot A
  leg_config "";  clean; build B;  snapshot B;  compare A B
  leg_config 1;   clean; build C;  snapshot C;  compare A C
  leg_config 64;  clean; build D;  snapshot D;  compare A D
  leg_config ""
  echo "-- determinism-check marker" >> "$LEAF"
  build EDIT; snapshot EDIT
  git checkout -- "$LEAF"
  build REVERT; snapshot REVERT; compare A REVERT
  echo; echo "result: $([ $fail -eq 0 ] && echo PASS || echo FAIL)  (hash lists and logs in $OUT)"
  exit $fail
}

case "${1-}" in
  hash) [ $# -eq 2 ] || usage; set -eo pipefail; hash_build "$2";;
  verdict) [ $# -ge 3 ] || usage; shift; verdict "$@";;
  run) [ $# -eq 1 ] || usage; run_ladder;;
  *) usage;;
esac
