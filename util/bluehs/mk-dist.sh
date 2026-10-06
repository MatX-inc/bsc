#!/usr/bin/env bash
# Install the bluehs distribution: a relocatable tree holding a pruned GHC
# runtime, a package store with the compiled bsc libraries and their
# dependencies, the SAT solver libraries, the tool entry scripts (the
# programs of bsc.cabal), and the bluehs launcher.  Scripts run against the
# libraries with no Haskell toolchain installed; a C compiler is needed only
# by scripts that use CPP.
#
# The tree is the same bytes whatever the checkout directory and the build's
# parallelism (the determinism ladder, doc/cabal.md section 5, checks it):
# the dependencies come from the cabal store, whose path the files built
# there record (a Paths_ module's directories, a shared object's search
# paths and build id), so that path must not be the checkout's; the solvers
# are the installed tree's, built by make from the vendored sources, which
# the library links by name with no directory recorded (SetupHooks.hs), and
# are stripped of the debug information that names their build directory;
# and only libraries are shipped, no build tool.
#
# The libraries are the cabal project's own packages (cabal.project): the
# package `bsc` and, once the compiler is split, the packages it depends on.
# The package set and the programs are read off the project, so a split, or
# a step that adds a program, changes nothing here.
#
# The install directory sits inside a bsc installation (<prefix>/bluehs),
# and must be built from the same tree as that bsc: the library embeds the
# build version and rejects .ba files written by any other bsc.
#
# Prerequisites: a completed `make install-src` into <prefix>, ghc and cabal
# on PATH (the GHC that will be shipped), python3, cpio, and patchelf on
# Linux.
#
# Usage: util/bluehs/mk-dist.sh <prefix>/bluehs [work-dir]
#        (`make install-bluehs` runs it with inst/bluehs and build/bluehs)
# REUSE_WORK=1 keeps a previous run's library build, which cabal then updates.

set -euo pipefail

# canonical, as the in-place registrations record the build directories under it
REPO=$(cd "$(dirname "$(readlink -f "$0")")/../.." && pwd -P)
[ $# -ge 1 ] || { echo "usage: $0 <prefix>/bluehs [work-dir]" >&2; exit 1; }
mkdir -p "$1"
DEST=$(cd "$1" && pwd)
PREFIX=$(dirname "$DEST")
# The solvers the installed compiler carries, built by make from the vendored
# sources: the library links them by name and records no directory
# (SetupHooks.hs), so the build below is pointed at them the way the make
# build's is, and the distribution's copies are these.
SOLVER_DIR=$PREFIX/lib/SAT
WORK=${2:-$REPO/build/bluehs}
mkdir -p "$WORK"
WORK=$(cd "$WORK" && pwd -P)
DIST=$WORK/dist

msg() { echo ">>> $*"; }
die() { echo "mk-dist.sh: $*" >&2; exit 1; }
# GNU and BSD sed disagree on -i
sed_inplace() { local f=$1; shift; sed "$@" "$f" > "$f.tmp" && mv "$f.tmp" "$f"; }
relpath() { python3 -c 'import os, sys; print(os.path.relpath(*sys.argv[1:]))' "$1" "$2"; }

case "$(uname -s)" in
    Linux)  DLL=so ;;
    Darwin) DLL=dylib ;;
    *) die "unsupported OS $(uname -s)" ;;
esac
command -v python3 >/dev/null || die "python3 is required"
command -v cpio >/dev/null || die "cpio is required"
[ $DLL = dylib ] || command -v patchelf >/dev/null || die "patchelf is required"
[ -x "$PREFIX/bin/bsc" ] || die "no bsc at $PREFIX/bin; run make install-src first"

# ----------------------------------------------------------------------
# 0. The GHC installation

GHC_REAL_BIN=$(dirname "$(readlink -f "$(command -v ghc)")")
# ghcup layout: <root>/bin/ghc -> symlink; real binaries in <root>/lib/ghc-*/bin
# or directly <root>/bin.  Find the root containing both bin/ and lib/.
GHC_ROOT=$(cd "$GHC_REAL_BIN/.." && pwd)
while [ ! -d "$GHC_ROOT/lib" ] || [ ! -d "$GHC_ROOT/bin" ]; do
    GHC_ROOT=$(dirname "$GHC_ROOT")
    [ "$GHC_ROOT" = "/" ] && die "cannot locate the GHC root"
done
GHC_VER=$(ghc --numeric-version)
msg "GHC $GHC_VER at $GHC_ROOT"

if [ "${REUSE_WORK:-0}" != 1 ]; then rm -rf "$WORK/proj"; fi
rm -rf "$DIST"
mkdir -p "$DIST"/{bin,scripts,SAT,LICENSES,hs}

# ----------------------------------------------------------------------
# 1. Build the libraries, and their dependencies into the cabal store.  The
#    project is the repository's own, copied with its package paths made
#    absolute and without its environment-file setting, and built from
#    another directory: the checkout's dist-newstyle and .ghc.environment.*
#    (what the launcher uses in a checkout) are untouched.  The libraries
#    themselves are built in place, not installed: installing a local
#    package goes through a source distribution, which lacks the vendored
#    solver sources the cabal hooks build.  The build version comes from
#    the hooks too (BuildVersion is generated into each build), so the
#    library and the bsc installed from this tree carry the same stamp.
#    The dependencies go into the store cabal uses for everything else, not
#    one under the checkout: what a package records of the store it was
#    built in would otherwise name the checkout.

msg "building the bsc libraries (this compiles every module at -O2)"
mkdir -p "$WORK/proj"
{
    echo "-- Generated by util/bluehs/mk-dist.sh from $REPO/cabal.project."
    # packages: either on one line or one package per indented line under it
    awk -v repo="$REPO" '
        function absolute(p) { sub(/^[ \t]+/, "", p); sub(/[ \t]+$/, "", p); return (p == "." ? repo : repo "/" p) }
        /^packages:/ {
            inpk = 1; rest = $0; sub(/^packages:[ \t]*/, "", rest)
            if (rest == "") { print; next }
            n = split(rest, ps, /[ \t,]+/); print "packages:"
            for (i = 1; i <= n; i++) if (ps[i] != "") print "  " absolute(ps[i])
            next
        }
        inpk && /^[ \t]+[^ \t]/ { print "  " absolute($0); next }
        inpk { inpk = 0 }
        /^write-ghc-environment-files:/ { next }
        { print }' "$REPO/cabal.project"
} > "$WORK/proj/cabal.project"
# The project pins the Hackage index its dependencies resolve against, so
# this build and the installed compiler's take the same versions
grep -qi '^index-state[[:space:]]*:' "$WORK/proj/cabal.project" || die "$REPO/cabal.project pins no index-state"
# bsc depends on every package a split produces, so this builds them all
(cd "$WORK/proj" && LIBRARY_PATH="$SOLVER_DIR${LIBRARY_PATH:+:$LIBRARY_PATH}" cabal build bsc:lib:bsc \
    >"$WORK/cabal-build.log" 2>&1) \
    || { tail -30 "$WORK/cabal-build.log" >&2; exit 1; }
PLAN=$WORK/proj/dist-newstyle/cache/plan.json
[ -f "$PLAN" ] || die "no build plan at $PLAN"

# cabal names the store's directory ghc-<version>, or in newer releases
# ghc-<version>-<abi hash>
STORE_ROOT=$(cd "$WORK/proj" && cabal path --store-dir)
STORE_SRC=
for d in "$STORE_ROOT/ghc-$GHC_VER" "$STORE_ROOT/ghc-$GHC_VER"-*; do
    [ -d "$d/package.db" ] && { STORE_SRC=$d; break; }
done
[ -n "$STORE_SRC" ] || die "no ghc-$GHC_VER store in $STORE_ROOT"
STORE_NAME=$(basename "$STORE_SRC")

# The in-place libraries: registration, id and build directory of every
# package of the project.  A library of re-exports alone has no modules of
# its own, and so no build directory to copy from.
INPLACE_CONFS=() INPLACE_IDS=() INPLACE_BUILDS=()
BSC_ID=
for conf in "$WORK/proj/dist-newstyle/packagedb/ghc-$GHC_VER"/*-inplace.conf; do
    name=$(sed -n 's/^name: *//p' "$conf")
    id=$(sed -n 's/^id: *//p' "$conf")
    build=$(awk '/^import-dirs:/ { getline; if ($0 ~ /^[ \t]/) print $1 }' "$conf")
    [ -z "$build" ] || [ -d "$build" ] || die "cannot locate the in-place build of $name ($build)"
    INPLACE_CONFS+=("$conf"); INPLACE_IDS+=("$id"); INPLACE_BUILDS+=("$build")
    [ "$name" != bsc ] || BSC_ID=$id
done
[ -n "$BSC_ID" ] || die "no in-place bsc package in the project"

# ----------------------------------------------------------------------
# 2. Pruned GHC runtime

msg "copying and pruning GHC runtime"
mkdir -p "$DIST/hs/ghc"
cp -a "$GHC_ROOT/bin" "$GHC_ROOT/lib" "$DIST/hs/ghc/"

# Drop profiling and static ways (keep libCffi*.a: shipped static-only,
# and deleting it makes `ghc-pkg check` report rts as broken)
find "$DIST/hs/ghc/lib" -name '*.p_hi' -delete
find "$DIST/hs/ghc/lib" -name '*_p.a' -delete
find "$DIST/hs/ghc/lib" -name '*.a' ! -name 'libCffi*' -delete
# Drop tools scripts never need
for tool in haddock hsc2hs hpc hp2ps ghc-iserv ghc-iserv-prof; do
    find "$DIST/hs/ghc/bin" -maxdepth 1 -name "$tool*" -delete
    find "$DIST/hs/ghc/lib" -path '*/bin/*' -name "$tool*" ! -name 'ghc-iserv-dyn*' -delete
done

# Replace the outer bin/ wrappers (they hardcode the build machine's
# paths) with self-locating equivalents.  ghc/ghc-pkg/runghc have real
# binaries under lib/ghc-<ver>/bin; ghci and runhaskell are pure aliases.
GHCLIBDIR_REL="lib/ghc-$GHC_VER/lib"
[ -d "$DIST/hs/ghc/$GHCLIBDIR_REL" ] || die "unexpected GHC layout"
rm -f "$DIST/hs/ghc/bin"/*
mkwrap() {  # name, exec-line
    printf '#!/bin/sh\nhere=$(dirname "$(readlink -f "$0")")\nroot=$(dirname "$here")\nexec %s "$@"\n' "$2" \
        > "$DIST/hs/ghc/bin/$1"
    chmod 755 "$DIST/hs/ghc/bin/$1"
}
mkwrap ghc     "\"\$root/lib/ghc-$GHC_VER/bin/ghc-$GHC_VER\" -B\"\$root/$GHCLIBDIR_REL\""
mkwrap ghc-pkg "\"\$root/lib/ghc-$GHC_VER/bin/ghc-pkg-$GHC_VER\" --global-package-db \"\$root/$GHCLIBDIR_REL/package.conf.d\""
mkwrap runghc  "\"\$root/lib/ghc-$GHC_VER/bin/runghc-$GHC_VER\" -f \"\$here/ghc\""
mkwrap runhaskell "\"\$here/runghc\""
mkwrap ghci    "\"\$here/ghc\" --interactive"

# GHC's loader looks up each library a package names by asking the C
# compiler (--print-file-name).  The system libraries the boot packages name
# (libc, libm, libgmp, ...) already load as dependencies of GHC and of the
# packages' shared objects, so dropping the names lets scripts run where no
# C compiler is reachable.
for conf in "$DIST/hs/ghc/$GHCLIBDIR_REL"/package.conf.d/*.conf; do
    awk 'BEGIN{skip=0} /^[^ \t]/{skip = /^extra-libraries:/ ? 1 : 0} !skip' \
        "$conf" > "$conf.tmp" && mv "$conf.tmp" "$conf"
done
"$DIST/hs/ghc/bin/ghc-pkg" recache --global

# ----------------------------------------------------------------------
# 3. Relocatable store

# The plan's library units that live in the store: the dependencies, as
# built for this project.  The store holds whatever else was ever built on
# this machine, and the plan's build tools (hsc2hs) and setup components,
# none of which a script needs.
msg "relocating package store"
STORE=$DIST/hs/store/$STORE_NAME
mkdir -p "$STORE/package.db"
python3 -c '
import json, sys
plan = json.load(open(sys.argv[1]))
for u in plan["install-plan"]:
    if u.get("style") == "global" and u.get("component-name", "lib") == "lib":
        print(u["id"])
' "$PLAN" | while read -r id; do
    [ -d "$STORE_SRC/$id" ] || die "the plan's $id is not in the store $STORE_SRC"
    cp -a "$STORE_SRC/$id" "$STORE/"
    cp "$STORE_SRC/package.db/$id.conf" "$STORE/package.db/"
    # On macOS cabal-install keeps every unit's dynamic library in the
    # store's one lib directory, to stay under the Mach-O load command size
    # limit (cabal-install's ProjectPlanning, dynlibdir), and the unit's
    # registration and the rpaths of what links it name that directory.
    for dl in "$STORE_SRC/lib/libHS$id-ghc"*."$DLL"; do
        [ -e "$dl" ] || continue
        mkdir -p "$STORE/lib" && cp -a "$dl" "$STORE/lib/"
    done
done
[ -n "$(ls "$STORE/package.db")" ] || die "no library of the plan found in $STORE_SRC"

find "$STORE" -name '*.a' -delete
find "$STORE" -name 'cabal-hash.txt' -delete

# The in-place libraries join the store as more packages: their interface
# files and shared objects, registered where the dependencies are
msg "adding the bsc libraries to the store"
for i in "${!INPLACE_IDS[@]}"; do
    id=${INPLACE_IDS[$i]} build=${INPLACE_BUILDS[$i]}
    mkdir -p "$STORE/$id/lib"
    if [ -n "$build" ]; then
        (cd "$build" && find . \( -name '*.hi' -o -name '*.dyn_hi' -o -name "libHS*.$DLL" \) -print0 \
            | cpio -0 -pdm --quiet "$STORE/$id/lib")
    fi
    # Keep the fields the packaged library needs; the include and link
    # settings of the build tree only served compiling it.  Of the C
    # libraries, only the solvers are named, by the package whose build
    # named them: they are in the distribution, where GHC finds them by
    # path.  The system libraries the libraries also use (zlib, Tcl, the
    # C++ runtime) load as dependencies of the shared objects and of the
    # solvers'; naming them would have GHC look each one up by its
    # development-package name, and ask the C compiler when that fails.
    # A field is its header line and the indented lines after it.
    awk '
        function flush() {
            if (block == "") return
            if (field ~ /^(include-dirs|ld-options|library-dirs-static|data-dir)$/) ;
            else if (field == "extra-libraries") {
                if (block ~ /[ \t\n](stp|yices)([ \t\n]|$)/) print "extra-libraries: stp yices"
            } else print block
            block = ""
        }
        /^[^ \t]/ { flush(); field = $0; sub(/:.*/, "", field); block = $0; next }
        { block = block "\n" $0 }
        END { flush() }' "${INPLACE_CONFS[$i]}" \
        > "$STORE/package.db/$id.conf"
    [ -z "$build" ] || sed_inplace "$STORE/package.db/$id.conf" -e "s|$build|\${pkgroot}/$id/lib|g"
done

# ${pkgroot} = the directory containing package.db
for conf in "$STORE"/package.db/*.conf; do
    sed_inplace "$conf" -e "s|$STORE_SRC|\${pkgroot}|g"
    # The solvers for GHC's loader, which opens a package's extra-libraries
    # by their link names from the package's library directories: the
    # registration names none (SetupHooks.hs), so the shipped one gets SAT.
    if [ "$conf" = "$STORE/package.db/$BSC_ID.conf" ]; then
        awk '{ print } /^(library-dirs|dynamic-library-dirs):/ { print "    ${pkgroot}/../../../SAT" }' \
            "$conf" > "$conf.tmp" && mv "$conf.tmp" "$conf"
    fi
    # Drop haddock-* fields INCLUDING their continuation lines (they point
    # at never-shipped docs; a bare sed of the header line would orphan the
    # indented value lines and break ghc-pkg's parser)
    awk 'BEGIN{skip=0} /^[^ \t]/{skip = /^haddock-(interfaces|html):/ ? 1 : 0} !skip' \
        "$conf" > "$conf.tmp" && mv "$conf.tmp" "$conf"
done
"$DIST/hs/ghc/bin/ghc-pkg" --package-db="$STORE/package.db" recache

# ----------------------------------------------------------------------
# 4. Package environment: the bsc package and everything it depends on,
#    boot packages included, and nothing else.  The store also holds the
#    build's setup dependencies (Cabal among them); exposed beside their
#    boot versions they would make every CPP script's version macros
#    ambiguous.

msg "writing package environment"
GLOBALDB=$(ls -d "$DIST/hs/ghc/lib/ghc-$GHC_VER/lib/package.conf.d")
{
    echo "clear-package-db"
    echo "global-package-db"
    echo "package-db store/$STORE_NAME/package.db"
    "$DIST/hs/ghc/bin/ghc-pkg" --package-db="$STORE/package.db" dump \
        | awk -v root="$BSC_ID" '
            /^id:/ { id = $2; next }
            /^depends:/ { field = "depends"; sub(/^depends:/, ""); addDeps(); next }
            /^[^ \t]/ { field = "" ; next }
            field == "depends" { addDeps() }
            function addDeps(   i) { for (i = 1; i <= NF; i++) deps[id] = deps[id] " " $i }
            END {
                queue[1] = root; n = 1; seen[root] = 1
                for (i = 1; i <= n; i++) {
                    print "package-id " queue[i]
                    split(deps[queue[i]], ds, " ")
                    for (j in ds) if (ds[j] != "" && !(ds[j] in seen)) { seen[ds[j]] = 1; queue[++n] = ds[j] }
                }
            }'
} > "$DIST/hs/bsc.env"

# ----------------------------------------------------------------------
# 5. SAT libs, scripts, launcher

msg "copying SAT libs, scripts and launcher"
cp -a "$SOLVER_DIR"/*."$DLL"* "$DIST/SAT/"
# Every program of bsc.cabal runs as a script except bluetcl, whose
# executable compiles in a C shim (bluetcl_shim.c) that hands control to
# Tcl, which an interpreted run cannot supply.  The entry file is named
# after the program, which is how the launcher finds it; the programs'
# hs-source-dirs may be a farm of links into src/, and cp follows them.
awk '
    function flush() { if (name != "" && name != "bluetcl") print name, dir, main; name = "" }
    /^executable[ \t]/ { flush(); name = $2; dir = "."; main = ""; next }
    /^[^ \t]/ { flush() }
    name != "" && $1 == "hs-source-dirs:" { dir = $2 }
    name != "" && $1 == "main-is:" { main = $2 }
    END { flush() }' "$REPO/bsc.cabal" \
| while read -r name dir main; do
    [ -n "$main" ] || die "bsc.cabal: executable $name has no main-is"
    cp "$REPO/$dir/$main" "$DIST/scripts/$name.hs"
done
[ -n "$(ls "$DIST/scripts")" ] || die "no programs found in bsc.cabal"
cp "$REPO/util/bluehs/bluehs" "$DIST/bin/bluehs"
chmod 755 "$DIST/bin/bluehs"

# ----------------------------------------------------------------------
# 6. Library paths: every search path and dependency that names the build
#    machine is rewritten relative to the file, or dropped

# The shipped counterpart of a build-machine directory; fails for one
# that has none.  Directories elsewhere (the system's) are kept as they are.
shipped_dir() {
    local i d
    case "$1" in
        "$STORE_SRC" | "$STORE_SRC"/*) echo "$STORE${1#"$STORE_SRC"}"; return ;;
        "$GHC_ROOT"/*) echo "$DIST/hs/ghc${1#"$GHC_ROOT"}"; return ;;
    esac
    for i in "${!INPLACE_IDS[@]}"; do
        [ -n "${INPLACE_BUILDS[$i]}" ] || continue
        case "$1" in
            "${INPLACE_BUILDS[$i]}"*)
                echo "$STORE/${INPLACE_IDS[$i]}/lib${1#"${INPLACE_BUILDS[$i]}"}"; return ;;
        esac
    done
    case "$1" in
        # the library's own rpath for the solvers (cabal.project): SAT here
        '$ORIGIN/../lib/SAT' | '@loader_path/../lib/SAT') echo "$DIST/SAT"; return ;;
        "$WORK"/* | "$REPO"/*) return 1 ;;
        /*) echo "$1" ;;
        *) return 1 ;;
    esac
}

# A search path as the file should record it
search_entry() {  # file, directory, origin token
    local d
    d=$(shipped_dir "$2") || return 1
    case "$d" in
        "$DIST"/*) echo "$3/$(relpath "$d" "$(dirname "$1")")" ;;
        *) echo "$d" ;;
    esac
}

relocate_elf() {
    local f=$1 r e new=()
    local IFS=:
    for r in $(patchelf --print-rpath "$f"); do
        e=$(search_entry "$f" "$r" '$ORIGIN') && new+=("$e")
    done
    patchelf --set-rpath "${new[*]}" "$f"
}

relocate_macho() {
    local f=$1 r e dep id
    for r in $(otool -l "$f" | awk '$1 == "cmd" { rp = ($2 == "LC_RPATH") } rp && $1 == "path" { print $2 }'); do
        install_name_tool -delete_rpath "$r" "$f"
        if e=$(search_entry "$f" "$r" @loader_path); then
            install_name_tool -add_rpath "$e" "$f" 2>/dev/null || true
        fi
    done
    id=$(otool -D "$f" | sed -n 2p)
    case "$id" in
        /*) shipped_dir "$(dirname "$id")" >/dev/null || install_name_tool -id "@rpath/$(basename "$id")" "$f" ;;
    esac
    for dep in $(otool -L "$f" | awk 'NR > 1 { print $1 }'); do
        [ "$dep" = "$id" ] && continue
        case "$(shipped_dir "$(dirname "$dep")" 2>/dev/null || echo dropped)" in
            "$DIST"/* | dropped)
                install_name_tool -change "$dep" "@rpath/$(basename "$dep")" "$f" ;;
        esac
    done
    codesign -f -s - "$f" 2>/dev/null
}

# Search paths and dependencies of a file that still name the build machine
leaks() {
    local f=$1
    if [ $DLL = so ]; then
        patchelf --print-rpath "$f"; patchelf --print-needed "$f"
    else
        otool -l "$f" | awk '$1 == "path" || $1 == "name" { print $2 }'
    fi | grep -F -e "$WORK" -e "$REPO" -e "$GHC_ROOT" -e "$STORE_SRC" || true
}

msg "rewriting library paths"
LIBS=()
while IFS= read -r -d '' f; do LIBS+=("$f"); done \
    < <(find "$STORE" "$DIST/SAT" -type f -name "*.$DLL*" -print0)
for f in "${LIBS[@]}"; do
    chmod u+w "$f"
    # The bsc library and the solvers carry symbols and debug information
    # no script needs; the solvers' names their build directory.
    case "$f" in
        */libHSbsc-* | */SAT/*) if [ $DLL = so ]; then strip "$f"; else strip -S -x "$f"; fi ;;
    esac
    if [ $DLL = so ]; then relocate_elf "$f"; else relocate_macho "$f"; fi
done
for f in "${LIBS[@]}"; do
    l=$(leaks "$f")
    [ -z "$l" ] || die "$f still names the build machine: $l"
done

# ----------------------------------------------------------------------
# 7. Licensing: everything redistributed in the distribution

msg "generating LICENSES"
cp "$REPO"/LICENSES/LICENSE.ghc "$DIST/LICENSES/"
cp "$REPO"/LICENSES/LICENSE.stp "$REPO"/LICENSES/LICENSE.stp_components \
   "$REPO"/LICENSES/LICENSE.yices "$DIST/LICENSES/"
# Per-package name/version/license/copyright for every Haskell package in
# the distribution (boot libraries + store deps + the bsc packages), via the
# transitive closure walker already used for the main tarball's
# LICENSE.ghc_pkgs, from the bsc package
PATH="$DIST/hs/ghc/bin:$PATH" \
GHC_PACKAGE_PATH="$STORE/package.db:$GLOBALDB" \
    "$REPO/src/comp/make-ghc-pkg-info.sh" bsc \
    > "$DIST/LICENSES/LICENSE.ghc_pkgs"

cat > "$DIST/LICENSES/COPYING" <<EOT
The bluehs distribution redistributes the following components:

  * The Glasgow Haskell Compiler runtime and tools (ghc, runghc,
    ghc-pkg, boot libraries, RTS, bundled libffi)
      - See LICENSES/LICENSE.ghc
  * Haskell library packages (GHC boot libraries, packages from
    Hackage, and the bsc compiler libraries themselves), enumerated with
    their licenses and copyrights in:
      - See LICENSES/LICENSE.ghc_pkgs
  * The STP SAT solver shared library (SAT/libstp.*)
      - See LICENSES/LICENSE.stp and LICENSES/LICENSE.stp_components
  * The Yices SMT solver shared library (SAT/libyices.*)
      - See LICENSES/LICENSE.yices

Not included, required from the host system at runtime: the C and C++
runtimes, libgmp, zlib, Tcl, and, for scripts that use CPP, a C compiler.
EOT

if [ ! -f "$GHC_ROOT/LICENSE" ]; then
    msg "NOTE: GHC installation carries no LICENSE file (ghcup layout);"
    msg "      shipped LICENSE.ghc is the repo's copy - release CI should"
    msg "      verify it matches the shipped GHC version's license text."
else
    cp "$GHC_ROOT/LICENSE" "$DIST/LICENSES/LICENSE.ghc"
fi

# ----------------------------------------------------------------------
# 8. Install, then smoke test a copy in another directory with the build
#    tree out of reach, under an empty environment and, for the scripts
#    that do not use CPP, with no C compiler on PATH

msg "installing into $DEST"
rm -rf "$DEST"
mv "$DIST" "$DEST"

msg "smoke testing"
SMOKE=$(mktemp -d)
cp -a "$DEST" "$SMOKE/bluehs"
cat > "$SMOKE/probe.hs" <<'EOT'
import Control.Monad (unless)
import qualified STP
import System.Exit (die)
import Version (bscVersionStr)
import qualified Yices
main :: IO ()
main = do
    _ <- Yices.checkVersion
    STP.checkVersion >>= flip unless (die "STP: version check failed")
    putStrLn (bscVersionStr True)
EOT
mv "$WORK" "$WORK.hidden"
trap 'mv "$WORK.hidden" "$WORK"; rm -rf "$SMOKE"' EXIT
# only what the launcher and the GHC wrappers use
mkdir "$SMOKE/bin"
for t in sh readlink dirname basename; do ln -s "$(command -v $t)" "$SMOKE/bin/$t"; done
run() { env -i PATH="$1" HOME=/nonexistent "$SMOKE/bluehs/bin/bluehs" "${@:2}"; }
# dumpbo uses CPP, which needs a C compiler
run /usr/bin:/bin dumpbo "$PREFIX/lib/Libraries/Prelude.bo" > "$SMOKE/dumpbo.out"
head -1 "$SMOKE/dumpbo.out" | grep -q "Internal Symbols" \
    || die "smoke test: dumpbo output unexpected"
LIBRARY=$(run "$SMOKE/bin" "$SMOKE/probe.hs")
COMPILER=$("$PREFIX/bin/bsc" -v | head -1)
[ "$LIBRARY" = "$COMPILER" ] \
    || die "the library and $PREFIX/bin/bsc differ: '$LIBRARY' vs '$COMPILER'"
msg "done: $DEST ($(du -sh "$DEST" | cut -f1)), $LIBRARY"
