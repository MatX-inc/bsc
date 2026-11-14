#!/usr/bin/env bash
# The determinism ladder of the cabal build (doc/cabal.md): one commit built
# serially, oversubscribed, in another directory or, from GHC 9.12,
# incrementally must give bit-identical objects, interfaces, archives, shared
# objects and programs, and the same bytes in any installed directory named to
# `hash` (`run` names the installed tree, inst, the bluehs runtime under it
# included).  `run` is the whole ladder on one machine, every leg a fresh
# checkout in a directory of its own: it runs `hash` on every leg, `rebuild` on
# the serial legs and `verdict` over the lists; CI
# (.github/workflows/cabal.yml) runs it, one job per platform.  `hash` writes a
# record of the toolchain beside each list, which `verdict` reads a difference
# against.
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

# What built the list it is written beside, for reading a difference: the
# runner image when GitHub's runner names one, the system, the C compiler,
# the linker and ar, ghc, cabal, patchelf where it is (mk-dist.sh rewrites the
# shipped libraries with it), and the versions in the package's
# cabal_macros.h, the header Cabal includes into every module that runs the C
# preprocessor: every program Cabal configured and found a version for (ld, ar
# and tar carry none), used by the package or not, and every dependency.
# A program installed at another version between two legs' dependency steps
# (pkg-config on a macOS runner, once) is in that header and so in the
# interface of every such module, in the package and in the store alike, with
# nothing in the build to blame; the record names it.  The dependencies are
# pinned by cabal.project's index-state and recorded for completeness.
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
  # The header's own comments name each tool and package with its version
  # (the macro names spell a hyphen as an underscore).
  if [ -d dist-newstyle/build ] && find dist-newstyle/build -name cabal_macros.h | grep -q .; then
    find dist-newstyle/build -name cabal_macros.h -exec sed -n \
      -e 's|^/\* tool \(.*\)-\([0-9][0-9.]*\) \*/$|tool \1: \2|p' \
      -e 's|^/\* package \(.*\)-\([0-9][0-9.]*\) \*/$|package \1: \2|p' {} + | LC_ALL=C sort -u
  else
    echo "cabal_macros.h: none under dist-newstyle/build"
  fi
  return 0
}

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
  # A list without interfaces, or an interface with the digest of empty text,
  # means the interface hashing silently produced nothing; it must not compare
  # IDENTICAL.  (An installed directory may hold empty files; those are bytes.)
  grep -q '\.hi$' "$LIST" || die "no .hi files hashed into $LIST"
  empty_digest="$(sha256 < /dev/null | cut -d' ' -f1)"
  grep "^$empty_digest " "$LIST" | grep -Eq '\.(hi|dyn_hi|p_hi)$' && die "$LIST hashes an interface as empty text"
  echo "$(wc -l < "$LIST") files hashed into $LIST"
  toolchain_record > "${LIST%.sha256}.toolchain"
  echo "toolchain recorded in ${LIST%.sha256}.toolchain"
}

# The toolchain records beside two lists (NAME.toolchain beside NAME.sha256):
# whether they agree and, when not, their difference, under the lists' line;
# with `quiet`, nothing when they agree or either is absent.
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

# Pass means every list is identical to the reference.  Differences are
# reported by category (program, shared object, object, interface, archive;
# installed for the files of an installed directory, and distribution for
# those of a bluehs runtime under it) so a failure says what moved, and
# against the toolchain records beside the two lists, so it says whether the
# same tools built them; the records do not decide the result, which is the
# bytes'.  Identical lists whose records differ are noted as well.
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
    toolchain_compare "$ref" "$other"
  done
  echo "result: $([ $fail -eq 0 ] && echo PASS || echo FAIL)"
  exit $fail
}

# The in-place round: a rebuild with nothing changed and, from GHC 9.12
# (ghc_deterministic), a rebuild after a marker edit, a comment, to one
# module, which recompiles that module alone, and a rebuild after the
# revert, each hashed like `hash` (with the DIRs given) into
# $OUT/NAME-rebuild, NAME-edit and NAME-revert.  $REBUILD_STEPS names the
# steps instead when set.  $BUILD_CMD is the build, `cabal build all`
# unless the caller installs too (`run` passes the make targets, so what the
# leg installs is rebuilt and hashed with the build).  The caller's EXIT trap
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

# The ladder on this machine.  A leg is a name DIR-jN: a fresh checkout of HEAD
# under $OUT/legs, in a directory of its own, built and installed at cabal
# jobs N (and ghc -jN where the project compiles in parallel, GHC 9.12 on)
# through the make targets, so what it installs is built and hashed with the
# build tree, then hashed with inst; a serial leg (N = 1) then runs the
# in-place round (rebuild_round).  The reference leg, the first of $LEGS,
# builds alone first: the dependency store it fills is shared by the others,
# which then run up to $LADDER_JOBS at once, one per 6 GB of memory unless
# set.  A leg's tree goes once its lists are written (KEEP_LEGS=1 keeps it);
# the lists, the toolchain records and the logs stay in $OUT, and the verdict
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
    || echo "GHC $(ghc --numeric-version): below 9.12 the project compiles each package serially (cabal.project), so the legs vary cabal's package jobs, not GHC's parallelism, and the in-place round is the rebuild with nothing changed alone"
  git diff --quiet || die "working tree is dirty; the legs check out HEAD"
  [ -e cabal.project.local ] && die "cabal.project.local exists; it would not reach the legs, which write their own"
  [ -f "$LEAF" ] || die "$LEAF not found"
  legs="${LEGS:-c-j1 a-j1 c-j16 c-j64 a-j64 d-j64}"; ref="${legs%% *}"
  jobs="${LADDER_JOBS:-$(ladder_jobs_default)}"
  ncpu=$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 4)
  mkdir -p "$OUT/legs"
  echo "ladder: legs $legs, reference $ref, up to $jobs at once; results in $OUT"
  # NAME: the leg's checkout, build and install, hash, and the serial leg's
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
      (cd "$dir" && ./util/cabal-ladder.sh hash "$OUT/$name.sha256" inst) >> "$OUT/$name.log" 2>&1 || rc=$?
    fi
    [ $rc -eq 0 ] && touch "$OUT/$name.built"
    if [ $rc -eq 0 ] && [ "$n" = 1 ]; then
      (cd "$dir" && OUT="$OUT" BUILD_CMD="$build" ./util/cabal-ladder.sh rebuild "$name" inst) >> "$OUT/$name.log" 2>&1 || rc=$?
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
