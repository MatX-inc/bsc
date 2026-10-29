#!/usr/bin/env bash
# Determinism ladder for the Cabal build (doc/cabal.md section 7): verify that
# serial, parallel, relocated, and in-place incremental builds of one commit
# produce identical objects, interfaces, archives, shared libraries, programs,
# and installed directories (`run` hashes inst/bluehs). `run` executes all
# legs in fresh checkouts on one machine, runs `rebuild` on the serial legs,
# and compares the hash lists with `verdict`. `hash` also writes a `.toolchain`
# record beside each list so `verdict` can report any tool changes.
set -u
SELF="$(cd "$(dirname "$0")" && pwd)/$(basename "$0")"
TOP="$(cd "$(dirname "$SELF")/.." && pwd)"; cd "$TOP" || exit 2
LEAF=src/comp/Libs/IOUtil.hs
usage() {
  cat >&2 <<USAGE
usage: util/cabal-ladder.sh hash OUT.sha256 [DIR...]
                                                  hash the build in dist-newstyle, and every file of
                                                  each installed DIR, into OUT.sha256, with the
                                                  toolchain record beside it in OUT.toolchain
       util/cabal-ladder.sh verdict REF OTHER...  compare hash lists against REF (an absent one is MISSING);
                                                  a difference is read against the toolchain records
       util/cabal-ladder.sh toolchain             print the toolchain record: the runner image, the system,
                                                  cc, ld, ar, ghc, cabal, patchelf where present and the
                                                  versions in cabal_macros.h
       util/cabal-ladder.sh rebuild NAME [DIR...]  rebuild in place with nothing changed and, from GHC
                                                  9.12, after a marker edit and its revert, each hashed
                                                  like \`hash\` into \$OUT/NAME-{rebuild,edit,revert}.sha256
                                                  (\$OUT default .); \$BUILD_CMD is the build
                                                  (default \`cabal build all\`)
       util/cabal-ladder.sh run                   the ladder on this machine: the legs \$LEGS (default the
                                                  six of doc/cabal.md), each a fresh checkout of HEAD under
                                                  \$OUT/legs, up to \$LADDER_JOBS at once (default one per
                                                  6 GB of memory), then the verdict; lists, records and
                                                  logs in \$OUT (default determinism-results/<stamp>);
                                                  KEEP_LEGS=1 keeps the legs' trees
USAGE
  exit 2
}
die() { echo "cabal-ladder: $*" >&2; exit 2; }
# GHC >= 9.12 provides -fobject-determinism (enabled in cabal.project), which
# makes compiled object files independent of the GHC session's unique supply so
# a single recompiled module matches a clean build. On older GHCs, object files
# depend on session uniques (e.g. local symbol names and SRT entry order), so
# only the no-op rebuild is checked in place.
ghc_deterministic() { ghc --numeric-version | grep -Eq '^(9\.1[2-9]|9\.[2-9][0-9]|[1-9][0-9])\.'; }
# macOS has no sha256sum; shasum prints the same "hash  path" lines.
sha256() { if command -v sha256sum >/dev/null 2>&1; then sha256sum "$@"; else shasum -a 256 "$@"; fi; }
# Hash interface files via `ghc --show-iface`, stripping `interface hash:` and
# `flags: fingerprint:` (which include Cabal's absolute autogen include path).
# Pass `-package-env -` so output does not depend on `.ghc.environment.*`.
iface_hash() {  # one interface file -> "hash  path"
  local text
  text="$(ghc -package-env - --show-iface "$1")" || { echo "cabal-ladder: ghc --show-iface failed on $1" >&2; exit 1; }
  [ -n "$text" ] || { echo "cabal-ladder: ghc --show-iface printed nothing for $1" >&2; exit 1; }
  printf '%s\n' "$text" | grep -v -e '^  interface hash:' -e '^  flags: fingerprint:' | sha256 | sed "s|  -\$|  $1|"
}
export -f sha256 iface_hash

# Record the host environment, build tools (cc, ld, ar, ghc, cabal, patchelf),
# and the tool and package versions from `cabal_macros.h` (which Cabal passes
# to every CPP-enabled module, so any tool version change alters interfaces).
toolchain_record() {
  version_line() {  # LABEL COMMAND ARGS...: the first line the command prints, or that it is not found
    if command -v "$2" >/dev/null 2>&1; then echo "$1: $("${@:2}" 2>&1 | head -1)"; else echo "$1: $2 not found"; fi
  }
  [ -n "${ImageOS-}${ImageVersion-}" ] && echo "image: ${ImageOS-} ${ImageVersion-}"
  echo "system: $(uname -srm)"
  version_line cc cc --version
  version_line ld ld -v
  version_line ar ar --version
  version_line ghc ghc --numeric-version
  version_line cabal cabal --numeric-version
  command -v patchelf >/dev/null 2>&1 && version_line patchelf patchelf --version
  # `cabal_macros.h` comments list each configured tool and dependency version.
  if [ -d dist-newstyle/build ] && find dist-newstyle/build -name cabal_macros.h | grep -q .; then
    find dist-newstyle/build -name cabal_macros.h -exec sed -n \
      -e 's|^/\* tool \(.*\)-\([0-9][0-9.]*\) \*/$|tool \1: \2|p' \
      -e 's|^/\* package \(.*\)-\([0-9][0-9.]*\) \*/$|package \1: \2|p' {} + | LC_ALL=C sort -u
  else
    echo "cabal_macros.h: none under dist-newstyle/build"
  fi
  return 0
}

# Hash objects, archives, shared libraries, interfaces, and executables under
# dist-newstyle/build plus all files in any given installed directories, using
# repo-relative paths. Interfaces under dist-newstyle/build and under `store/`
# in an installed directory are hashed with `iface_hash`; symlinks are hashed
# by their link target text.
hash_build() {
  LIST="$1"; shift
  # Remove incomplete outputs on failure so they cannot compare as valid.
  trap '[ $? -eq 0 ] || rm -f "$LIST" "${LIST%.sha256}.toolchain"' EXIT
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
  # Fail if no interfaces were hashed or if any interface hashed as empty text.
  grep -q '\.hi$' "$LIST" || die "no .hi files hashed into $LIST"
  empty_digest="$(sha256 < /dev/null | cut -d' ' -f1)"
  grep "^$empty_digest " "$LIST" | grep -Eq '\.(hi|dyn_hi|p_hi)$' && die "$LIST hashes an interface as empty text"
  echo "$(wc -l < "$LIST") files hashed into $LIST"
  toolchain_record > "${LIST%.sha256}.toolchain"
  echo "toolchain recorded in ${LIST%.sha256}.toolchain"
}

# Compare NAME.toolchain records alongside two hash lists.
# With `quiet`, print nothing when they agree or either is absent.
toolchain_compare() {  # REF_LIST OTHER_LIST [quiet]
  local a="${1%.sha256}.toolchain" b="${2%.sha256}.toolchain" f
  for f in "$a" "$b"; do
    [ -r "$f" ] && continue
    [ "${3-}" = quiet ] || echo "   toolchain: no record $f"
    return 0
  done
  if diff -q "$a" "$b" >/dev/null; then
    [ "${3-}" = quiet ] || echo "   toolchain: the same ($(basename "$a"), $(basename "$b"))"
    return 0
  fi
  echo "   toolchain: differs ($(basename "$a") < > $(basename "$b"))"
  diff "$a" "$b" | grep '^[<>]' | sed 's/^/      /'
}

# Compare each hash list against REF_LIST, grouping differences by artifact
# category (program, shared, object, interface, archive, distribution) and
# comparing sidecar .toolchain records.
verdict() {
  local ref="$1" fail=0 other name diffs n cat sel c; shift
  [ -r "$ref" ] && [ -s "$ref" ] || { echo "verdict: reference $ref is missing, unreadable or empty"; exit 2; }
  for other in "$@"; do
    name="$(basename "$other" .sha256)"
    if [ ! -r "$other" ] || [ ! -s "$other" ]; then echo "MISSING: $name (no hash list)"; fail=1; continue; fi
    if diff -q "$ref" "$other" >/dev/null; then
      echo "IDENTICAL: $(basename "$ref" .sha256) vs $name ($(wc -l < "$other") files)"
      toolchain_compare "$ref" "$other" quiet; continue
    fi
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
    toolchain_compare "$ref" "$other"
  done
  echo "result: $([ $fail -eq 0 ] && echo PASS || echo FAIL)"
  exit $fail
}

# In-place rebuild round: no-op rebuild, plus (on GHC >= 9.12) a comment edit
# to $LEAF and revert, hashing each step into $OUT/NAME-{rebuild,edit,revert}.sha256.
# Override steps with $REBUILD_STEPS and the build command with $BUILD_CMD.
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

# Run the full ladder in $OUT/legs. Each leg DIR-jN is a detached worktree at
# HEAD built at parallelism N and hashed with inst/bluehs; serial legs (N=1)
# also run rebuild_round. The reference leg (first in $LEGS) finishes its
# initial build before the remaining legs run (up to $LADDER_JOBS at once,
# defaulting to 1 per 6 GB RAM) so they share the populated Cabal store.
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
    || echo "GHC $(ghc --numeric-version): below 9.12 the project compiles each package serially (cabal.project), so the legs vary cabal's package jobs, not GHC's parallelism, and the in-place round is the rebuild with nothing changed alone"
  git diff --quiet || die "working tree is dirty; the legs check out HEAD"
  [ -e cabal.project.local ] && die "cabal.project.local exists; it would not reach the legs, which write their own"
  [ -f "$LEAF" ] || die "$LEAF not found"
  legs="${LEGS:-c-j1 a-j1 c-j16 c-j64 a-j64 d-j64}"; ref="${legs%% *}"
  jobs="${LADDER_JOBS:-$(ladder_jobs_default)}"
  ncpu=$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 4)
  mkdir -p "$OUT/legs"
  echo "ladder: legs $legs, reference $ref, up to $jobs at once; results in $OUT"
  # Check out, build, install, and hash leg NAME (plus rebuild_round for j1).
  # Touches $OUT/NAME.built once hashed and writes exit code to $OUT/NAME.rc.
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
    if [ $rc -eq 0 ] && [ "$n" = 1 ]; then
      (cd "$dir" && OUT="$OUT" BUILD_CMD="$build" ./util/cabal-ladder.sh rebuild "$name" inst/bluehs) >> "$OUT/$name.log" 2>&1 || rc=$?
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
  # Build the reference leg alone first to populate the shared store.
  leg_run "$ref" & refpid=$!
  while [ ! -e "$OUT/$ref.built" ] && kill -0 "$refpid" 2>/dev/null; do sleep 10; done
  [ -e "$OUT/$ref.built" ] || { wait "$refpid"; die "the reference leg $ref failed; see $OUT/$ref.log"; }
  local pids="$refpid"
  for name in ${legs#"$ref"}; do
    while [ "$(live $pids)" -ge "$jobs" ]; do sleep 10; done
    leg_run "$name" & pids="$pids $!"
  done
  wait
  # Collect expected hash lists so any failed leg or round reports MISSING.
  local others="" rounds=""
  for name in ${legs#"$ref"}; do others="$others $OUT/$name.sha256"; done
  for name in $legs; do
    [ "${name##*-j}" = 1 ] || continue
    rounds="$rounds $OUT/$name-rebuild.sha256"
    ghc_deterministic && rounds="$rounds $OUT/$name-revert.sha256"
  done
  echo; echo "toolchain of $ref:"; sed 's/^/   /' "$OUT/$ref.toolchain"
  echo "verdict against $ref (lists, records and logs in $OUT):"
  "$SELF" verdict "$OUT/$ref.sha256" $others $rounds
}

case "${1-}" in
  hash) [ $# -ge 2 ] || usage; set -eo pipefail; shift; hash_build "$@";;
  verdict) [ $# -ge 3 ] || usage; shift; verdict "$@";;
  toolchain) [ $# -eq 1 ] || usage; toolchain_record;;
  run) [ $# -eq 1 ] || usage; run_ladder;;
  rebuild) [ $# -ge 2 ] || usage; shift; OUT="${OUT:-.}"; trap 'git checkout -- "$LEAF"' EXIT; rebuild_round "$@";;
  *) usage;;
esac
