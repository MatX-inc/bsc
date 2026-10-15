#!/usr/bin/env bash
# The determinism ladder of the cabal build (doc/cabal.md): one commit built
# serially, oversubscribed, in another directory or, from GHC 9.12,
# incrementally must give
# bit-identical objects, interfaces, archives, shared objects and programs, and the same
# bytes in any installed directory named to `hash` (CI names the installed
# tree, inst, the bluehs runtime under it included).  CI (.github/workflows/cabal.yml) runs `hash` on
# every leg, `rebuild` on the serial legs and `verdict` over the lists; `run`
# is the whole ladder on one machine.
set -u
SELF="$(cd "$(dirname "$0")" && pwd)/$(basename "$0")"
TOP="$(cd "$(dirname "$SELF")/.." && pwd)"; cd "$TOP" || exit 2
LEAF=src/comp/Libs/IOUtil.hs
usage() {
  cat >&2 <<USAGE
usage: util/cabal-ladder.sh hash OUT.sha256 [DIR...]
                                                  hash the build in dist-newstyle, and every file of
                                                  each installed DIR, into OUT.sha256
       util/cabal-ladder.sh verdict REF OTHER...  compare hash lists against REF (an absent one is MISSING)
       util/cabal-ladder.sh rebuild NAME [DIR...]  rebuild in place with nothing changed and, from GHC
                                                  9.12, after a marker edit and its revert, each hashed
                                                  like \`hash\` into \$OUT/NAME-{rebuild,edit,revert}.sha256
                                                  (\$OUT default .); \$BUILD_CMD is the build
                                                  (default \`cabal build all\`)
       util/cabal-ladder.sh run                   the ladder on this machine; lists and logs in \$OUT
                                                  (default determinism-results/<stamp>)
USAGE
  exit 2
}
die() { echo "cabal-ladder: $*" >&2; exit 2; }
# From GHC 9.12 -fobject-determinism makes a module's object independent of
# the session's uniques (at 9.14.1, a module recompiled alone under a
# perturbed unique supply is identical with the flag and differs without
# it), and cabal.project turns it on there, so the in-place round holds the
# rebuild after the marker edit's revert to the reference.  Below 9.12 the
# flag does not exist and the object carries the uniques (at 9.4.7, in a
# local label's name and its relocation; in a small test, in the order of a
# static reference table's entries), so a module recompiled alone is not
# byte-identical to the one compiled in the full build, while clean builds
# agree among themselves.
ghc_deterministic() { ghc --numeric-version | grep -Eq '^(9\.1[2-9]|9\.[2-9][0-9]|[1-9][0-9])\.'; }
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

# Every object, archive, shared object and interface under dist-newstyle/build
# and every program bsc.cabal declares, as bytes except interfaces, with paths relative
# to the repository so builds in different directories compare.  Then every
# file of each installed directory named after the list: as bytes, except the
# interfaces under a `store` directory (the libraries this tree builds and
# their dependencies, whose bytes carry the build directory as above); a
# shipped GHC runtime's own interfaces are copies of the installed compiler's
# and compare as bytes.  A symbolic link is hashed as its target's text.
hash_build() {
  LIST="$1"; shift
  # A partial list would compare like a complete one.
  trap '[ $? -eq 0 ] || rm -f "$LIST"' EXIT
  local ncpu programs e bin empty_digest
  ncpu=$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 4)
  programs=$(awk '/^executable[ \t]/ {print $2}' bsc.cabal)
  [ -n "$programs" ] || die "no executable stanzas in bsc.cabal"
  {
    find dist-newstyle/build -type f \( -name '*.o' -o -name '*.dyn_o' -o -name '*.p_o' -o -name '*.a' -o -name '*.so' -o -name '*.dylib' \) -print0 \
      | sort -z | xargs -0 bash -eo pipefail -c 'sha256 "$@"' _
    find dist-newstyle/build -type f \( -name '*.hi' -o -name '*.dyn_hi' -o -name '*.p_hi' \) -print0 \
      | xargs -0 -n 1 -P "$ncpu" bash -eo pipefail -c 'iface_hash "$1"' _
    for e in $programs; do
      bin=$(cabal list-bin "bsc:exe:$e" 2>/dev/null) || continue
      [ -f "$bin" ] || continue
      sha256 "$bin"
    done
    for dir in "$@"; do
      [ -d "$dir" ] || die "$dir: no such directory"
      find "$dir" -type f ! \( -path '*/store/*' \( -name '*.hi' -o -name '*.dyn_hi' -o -name '*.p_hi' \) \) -print0 \
        | sort -z | xargs -0 bash -eo pipefail -c 'sha256 "$@"' _
      find "$dir" -type f -path '*/store/*' \( -name '*.hi' -o -name '*.dyn_hi' -o -name '*.p_hi' \) -print0 \
        | xargs -0 -n 1 -P "$ncpu" bash -eo pipefail -c 'iface_hash "$1"' _
      find "$dir" -type l -print0 | sort -z | while IFS= read -r -d '' l; do
        printf '%s' "$(readlink "$l")" | sha256 | sed "s|  -\$|  $l|"
      done
    done
  } | sed "s|  $TOP/|  |" | LC_ALL=C sort -k2 > "$LIST"
  for dir in "$@"; do
    grep -q "  ${dir#"$TOP/"}/" "$LIST" || die "no files of $dir hashed into $LIST"
  done
  # A list without interfaces, or an interface with the digest of empty text,
  # means the interface hashing silently produced nothing; it must not compare
  # IDENTICAL.  (An installed directory may hold empty files; those are bytes.)
  grep -q '\.hi$' "$LIST" || die "no .hi files hashed into $LIST"
  empty_digest="$(sha256 < /dev/null | cut -d' ' -f1)"
  grep "^$empty_digest " "$LIST" | grep -Eq '\.(hi|dyn_hi|p_hi)$' && die "$LIST hashes an interface as empty text"
  echo "$(wc -l < "$LIST") files hashed into $LIST"
}

# Pass means every list is identical to the reference.  Differences are
# reported by category (program, shared object, object, interface, archive;
# installed for the files of an installed directory, and distribution for
# those of a bluehs runtime under it) so a failure says what moved.
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
    build=$(printf '%s\n' "$diffs" | grep '^dist-newstyle/' || true)
    for cat in program shared object interface archive installed distribution; do
      case "$cat" in
        program) sel=$(printf '%s\n' "$build" | grep -Ev '\.(o|hi|dyn_o|dyn_hi|p_o|p_hi|a|so|dylib)$' || true);;
        shared) sel=$(printf '%s\n' "$build" | grep -E '\.(so|dylib)$' || true);;
        object) sel=$(printf '%s\n' "$build" | grep -E '\.(o|dyn_o|p_o)$' || true);;
        interface) sel=$(printf '%s\n' "$build" | grep -E '\.(hi|dyn_hi|p_hi)$' || true);;
        archive) sel=$(printf '%s\n' "$build" | grep '\.a$' || true);;
        installed) sel=$(printf '%s\n' "$diffs" | grep -v '^dist-newstyle/' | grep -v '/bluehs/' || true);;
        distribution) sel=$(printf '%s\n' "$diffs" | grep -v '^dist-newstyle/' | grep '/bluehs/' || true);;
      esac
      c=$(printf '%s\n' "$sel" | sed '/^$/d' | wc -l)
      [ "$c" -gt 0 ] && { echo "   $cat: $c"; printf '%s\n' "$sel" | sed '/^$/d' | head -8 | sed 's/^/      /'; }
    done
  done
  echo "result: $([ $fail -eq 0 ] && echo PASS || echo FAIL)"
  exit $fail
}

# The in-place round: a rebuild with nothing changed and, from GHC 9.12
# (ghc_deterministic), a rebuild after a marker edit, a comment, to one
# module, which recompiles that module alone, and a rebuild after the
# revert, each hashed like `hash` (with the DIRs given) into
# $OUT/NAME-rebuild, NAME-edit and NAME-revert.  $REBUILD_STEPS names the
# steps instead when set (CI passes its plan's, so the lists made are the
# lists its verdict expects).  $BUILD_CMD is the build, `cabal build all`
# unless the caller installs too (CI runs the make targets, so what the leg
# installs is rebuilt and hashed with the build).  The caller's EXIT trap
# restores $LEAF.
rebuild_round() {  # NAME [DIR...]
  local name="$1" steps="${REBUILD_STEPS:-}" step t0 rc; shift
  mkdir -p "$OUT"
  [ -f "$LEAF" ] || die "$LEAF not found"
  git diff --quiet -- "$LEAF" || die "$LEAF is modified; the round edits and reverts it"
  if [ -z "$steps" ]; then
    if ghc_deterministic; then steps="rebuild edit revert"
    else steps=rebuild; echo "GHC $(ghc --numeric-version): below 9.12 the round is the rebuild with nothing changed alone"; fi
  fi
  for step in $steps; do
    case $step in
      edit) echo "-- determinism-check marker" >> "$LEAF";;
      revert) git checkout -- "$LEAF";;
    esac
    t0=$(date +%s); rc=0
    bash -c "${BUILD_CMD:-cabal build all}" > "$OUT/$name-$step.log" 2>&1 || rc=$?
    echo "$name-$step: exit $rc, $(( $(date +%s) - t0 )) s"
    [ $rc -eq 0 ] || { tail -30 "$OUT/$name-$step.log"; die "$name-$step build failed"; }
    "$SELF" hash "$OUT/$name-$step.sha256" "$@" || die "$name-$step: hashing the build failed"
  done
}

# Clean builds with the project's settings (A, B), the in-place round on B
# (a rebuild of nothing and, from GHC 9.12, a marker edit of one module and
# a rebuild, the revert and a rebuild) whose first and last lists must match
# A, then serial (C) and oversubscribed (D).
run_ladder() {
  OUT="${OUT:-$TOP/determinism-results/$(date -u +%Y%m%dT%H%M%SZ)}"; mkdir -p "$OUT"
  command -v cabal >/dev/null || die "cabal not found"
  command -v ghc >/dev/null || die "ghc not found"
  ghc_deterministic \
    || echo "GHC $(ghc --numeric-version): below 9.12 the project compiles each package serially (cabal.project), so the legs vary cabal's package jobs, not GHC's parallelism, and the in-place round is the rebuild with nothing changed alone"
  git diff --quiet || die "working tree is dirty; commit or stash first"
  [ -e cabal.project.local ] && die "cabal.project.local exists; the ladder writes its own"
  [ -f "$LEAF" ] || die "$LEAF not found"
  # The solvers the library links by name (doc/cabal.md section 2): installed
  # once, outside the directories the legs clean, and named to the linker.
  make install-solvers > "$OUT/solvers.log" 2>&1 || { tail -20 "$OUT/solvers.log"; die "installing the solvers failed"; }
  export LIBRARY_PATH="$TOP/inst/lib/SAT${LIBRARY_PATH:+:$LIBRARY_PATH}"
  snapshot() { "$SELF" hash "$OUT/$1.sha256" || die "$1: hashing the build failed"; }
  compare() { "$SELF" verdict "$OUT/$1.sha256" "$OUT/$2.sha256" || fail=1; }
  clean() { rm -rf dist-newstyle/build dist-newstyle/cache dist-newstyle/packagedb dist-newstyle/tmp; }
  leg_config() {  # N: cabal jobs N, and ghc -jN where the project compiles in parallel (GHC 9.12 on; a later -j wins); empty = project defaults
    if [ -n "$1" ]; then printf 'jobs: %s\nif impl(ghc >= 9.12)\n  program-options\n    ghc-options: -j%s\n' "$1" "$1" > cabal.project.local
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
  rebuild_round B; compare A B-rebuild; if ghc_deterministic; then compare A B-revert; fi
  leg_config 1;   clean; build C;  snapshot C;  compare A C
  leg_config 64;  clean; build D;  snapshot D;  compare A D
  echo; echo "result: $([ $fail -eq 0 ] && echo PASS || echo FAIL)  (hash lists and logs in $OUT)"
  exit $fail
}

case "${1-}" in
  hash) [ $# -ge 2 ] || usage; set -eo pipefail; shift; hash_build "$@";;
  verdict) [ $# -ge 3 ] || usage; shift; verdict "$@";;
  run) [ $# -eq 1 ] || usage; run_ladder;;
  rebuild) [ $# -ge 2 ] || usage; shift; OUT="${OUT:-.}"; trap 'git checkout -- "$LEAF"' EXIT; rebuild_round "$@";;
  *) usage;;
esac
