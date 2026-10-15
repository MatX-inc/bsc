#!/usr/bin/env bash
# The determinism ladder of the cabal build (doc/cabal.md): one commit built
# serially, oversubscribed, in another directory or, from GHC 9.12,
# incrementally must give bit-identical objects, interfaces, archives, shared
# objects and programs, and the same bytes in any installed directory named to
# `hash` (`run` names the bluehs runtime it installs, inst/bluehs).  `run` is
# the whole ladder on one machine, every leg a fresh checkout in a directory of
# its own: it runs `hash` on every leg and `verdict` over the lists; CI
# (.github/workflows/cabal.yml) runs it, one job per platform.
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
       util/cabal-ladder.sh run                   the ladder on this machine: the legs \$LEGS (default the
                                                  six of doc/cabal.md), each a fresh checkout of HEAD under
                                                  \$OUT/legs, up to \$LADDER_JOBS at once (default one per
                                                  6 GB of memory), then the verdict; lists and
                                                  logs in \$OUT (default determinism-results/<stamp>);
                                                  KEEP_LEGS=1 keeps the legs' trees
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
# reported by category (program, shared object, object, interface, archive,
# and distribution for the files of an installed directory) so a failure says
# what moved.
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
    for cat in program shared object interface archive distribution; do
      case "$cat" in
        program) sel=$(printf '%s\n' "$build" | grep -Ev '\.(o|hi|dyn_o|dyn_hi|p_o|p_hi|a|so|dylib)$' || true);;
        shared) sel=$(printf '%s\n' "$build" | grep -E '\.(so|dylib)$' || true);;
        object) sel=$(printf '%s\n' "$build" | grep -E '\.(o|dyn_o|p_o)$' || true);;
        interface) sel=$(printf '%s\n' "$build" | grep -E '\.(hi|dyn_hi|p_hi)$' || true);;
        archive) sel=$(printf '%s\n' "$build" | grep '\.a$' || true);;
        distribution) sel=$(printf '%s\n' "$diffs" | grep -v '^dist-newstyle/' || true);;
      esac
      c=$(printf '%s\n' "$sel" | sed '/^$/d' | wc -l)
      [ "$c" -gt 0 ] && { echo "   $cat: $c"; printf '%s\n' "$sel" | sed '/^$/d' | head -8 | sed 's/^/      /'; }
    done
  done
  echo "result: $([ $fail -eq 0 ] && echo PASS || echo FAIL)"
  exit $fail
}

# The ladder on this machine.  A leg is a name DIR-jN: a fresh checkout of HEAD
# under $OUT/legs, in a directory of its own, built and installed at cabal
# jobs N (and ghc -jN where the project compiles in parallel, GHC 9.12 on)
# through the make targets, so what it installs is built and hashed with the
# build tree, then hashed with inst/bluehs; the reference then
# rebuilds after a marker edit to $LEAF and after its revert, each hashed,
# and the reverted rebuild is held to the reference.  The reference leg, the first of $LEGS,
# builds alone first: the dependency store it fills is shared by the others,
# which then run up to $LADDER_JOBS at once, one per 6 GB of memory unless
# set.  A leg's tree goes once its lists are written (KEEP_LEGS=1 keeps it);
# the lists and the logs stay in $OUT, and the verdict
# over them is the result.
ladder_jobs_default() {
  local kb
  kb=$(awk '/^MemTotal:/ {print $2}' /proc/meminfo 2>/dev/null)
  [ -n "$kb" ] || kb=$(( $(sysctl -n hw.memsize 2>/dev/null || echo 0) / 1024 ))
  kb=$(( kb / (6 * 1024 * 1024) )); [ "$kb" -ge 1 ] && echo "$kb" || echo 1
}
run_ladder() {
  OUT="${OUT:-$TOP/determinism-results/$(date -u +%Y%m%dT%H%M%SZ)}"; mkdir -p "$OUT"; OUT="$(cd "$OUT" && pwd)"
  local t legs ref jobs ncpu name pid refpid
  for t in cabal ghc git make; do command -v "$t" >/dev/null || die "$t not found"; done
  ghc_deterministic \
    || echo "GHC $(ghc --numeric-version): below 9.12 the project compiles each package serially (cabal.project), so the legs vary cabal's package jobs, not GHC's parallelism"
  git diff --quiet || die "working tree is dirty; the legs check out HEAD"
  [ -e cabal.project.local ] && die "cabal.project.local exists; it would not reach the legs, which write their own"
  [ -f "$LEAF" ] || die "$LEAF not found"
  legs="${LEGS:-c-j1 a-j1 c-j16 c-j64 a-j64 d-j64}"; ref="${legs%% *}"
  jobs="${LADDER_JOBS:-$(ladder_jobs_default)}"
  ncpu=$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 4)
  mkdir -p "$OUT/legs"
  echo "ladder: legs $legs, reference $ref, up to $jobs at once; results in $OUT"
  # NAME: the leg's checkout, build and install, hash, and the reference's
  # round; $OUT/NAME.built marks the store filled, $OUT/NAME.rc its exit.
  leg_run() {
    local name="$1" dir n rc=0 t0 build
    dir="$OUT/legs/$name"; n="${name##*-j}"
    { git worktree add --detach "$dir" HEAD && git -C "$dir" submodule update --init --recursive; } > "$OUT/$name.checkout.log" 2>&1 \
      || { echo "$name: checkout failed ($OUT/$name.checkout.log)"; echo 1 > "$OUT/$name.rc"; return 1; }
    printf 'jobs: %s\nif impl(ghc >= 9.12)\n  program-options\n    ghc-options: -j%s\n' "$n" "$n" > "$dir/cabal.project.local"
    build="make -j$ncpu GHCJOBS=$n install-src && make -C src/comp GHCJOBS=$n install-extra && make install-bluehs"
    t0=$(date +%s)
    (cd "$dir" && bash -c "$build") > "$OUT/$name.log" 2>&1 || rc=$?
    echo "$name: build exit $rc, $(( $(date +%s) - t0 )) s"
    if [ $rc -eq 0 ]; then
      (cd "$dir" && ./inst/bin/bsc -v) >> "$OUT/$name.log" 2>&1 || true
      (cd "$dir" && ./util/cabal-ladder.sh hash "$OUT/$name.sha256" inst/bluehs) >> "$OUT/$name.log" 2>&1 || rc=$?
    fi
    [ $rc -eq 0 ] && touch "$OUT/$name.built"
    if [ $rc -eq 0 ] && [ "$name" = "$ref" ]; then
      # The incremental round: a marker edit to one module, a rebuild, the
      # revert and a rebuild, each hashed; the reverted one must match.
      (cd "$dir" && echo "-- determinism-check marker" >> "$LEAF" && bash -c "$build" && ./util/cabal-ladder.sh hash "$OUT/$name-edit.sha256" inst/bluehs \
         && git checkout -- "$LEAF" && bash -c "$build" && ./util/cabal-ladder.sh hash "$OUT/$name-revert.sha256" inst/bluehs) >> "$OUT/$name.log" 2>&1 || rc=$?
      echo "$name: round exit $rc"
    fi
    [ $rc -eq 0 ] || tail -15 "$OUT/$name.log" | sed "s/^/   $name: /"
    [ -n "${KEEP_LEGS:-}" ] || { rm -rf "$dir"; git worktree prune; }
    echo $rc > "$OUT/$name.rc"
    return $rc
  }
  live() {  # how many of the pids given are running
    local p c=0; for p in "$@"; do kill -0 "$p" 2>/dev/null && c=$((c + 1)); done; echo $c
  }
  # The reference first, alone, until its lists are written or it fails.
  leg_run "$ref" & refpid=$!
  while [ ! -e "$OUT/$ref.built" ] && kill -0 "$refpid" 2>/dev/null; do sleep 10; done
  [ -e "$OUT/$ref.built" ] || { wait "$refpid"; die "the reference leg $ref failed; see $OUT/$ref.log"; }
  local pids="$refpid"
  for name in ${legs#"$ref"}; do
    while [ "$(live $pids)" -ge "$jobs" ]; do sleep 10; done
    leg_run "$name" & pids="$pids $!"
  done
  wait
  # Every leg and every round the plan expects, so a failed one is MISSING.
  local others="" rounds=""
  for name in ${legs#"$ref"}; do others="$others $OUT/$name.sha256"; done
  rounds="$OUT/$ref-revert.sha256"
  echo; echo "verdict against $ref (lists and logs in $OUT):"
  "$SELF" verdict "$OUT/$ref.sha256" $others $rounds
}

case "${1-}" in
  hash) [ $# -ge 2 ] || usage; set -eo pipefail; shift; hash_build "$@";;
  verdict) [ $# -ge 3 ] || usage; shift; verdict "$@";;
  run) [ $# -eq 1 ] || usage; run_ladder;;
  *) usage;;
esac
