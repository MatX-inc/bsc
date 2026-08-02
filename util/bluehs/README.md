# Haskell scripts against the bsc library (proof of concept)

This directory contains script versions of the bsc utility programs. Instead
of compiling `dumpbo`, `dumpba`, `bsc2bsv`, `bsv2bsc`, `showrules` and
`vcdcheck` to standalone binaries, each is an ordinary Haskell source file
executed by `runghc` against the **compiled** `bsc` library (see `bsc.cabal`
at the repo root). GHCi works the same way, giving an interactive environment
with the whole compiler importable.

The scripts are byte-identical to their `src/comp/*.hs` counterparts except
for the module header (`module Main_dumpbo(main)` → `module Main(main)`) and
the leading comment block.

## Setup

    util/bluehs/setup.sh

This regenerates `BuildVersion.hs`/`BuildSystem.hs`, writes the machine-local
`cabal.project.local` (absolute solver-library paths — ghc-pkg refuses
relative ones), and runs `cabal build` (first build compiles all 240 library
modules; incremental afterwards). It requires the vendored solver libraries,
which a normal `make install-src` has already built.

**Re-run `setup.sh` whenever you rebuild bsc at a new commit.** This is a
correctness requirement, not hygiene: `.bo`/`.ba` files embed the build
version string (git hash), and the library rejects `.ba` files whose stamp
differs from its own (`EBinFileVerMismatch`). The library and the `bsc` that
produced your files must share a `BuildVersion.hs`.

## Running scripts

Three equivalent ways, most convenient first:

    # from anywhere — the launcher sets GHC_ENVIRONMENT and BLUESPECDIR:
    util/bluehs/bin/dumpbo foo.bo
    util/bluehs/bin/showrules -o out.vcd mkTop dump.vcd

    # from inside the repo — GHC finds .ghc.environment.* by upward search:
    runghc util/bluehs/dumpba.hs foo.ba
    ./util/bluehs/bsv2bsc.hs Foo.bsv          # shebang execution

    # explicit project context:
    cabal exec -v0 -- runghc util/bluehs/bsc2bsv.hs Foo.bs

Startup cost is ~0.3–0.5s (the library is compiled; only the script itself is
interpreted).

## Interactive use

    ghci        # anywhere inside the repo (but see .ghci caveat below)
    ghci> :m + GenBin ISyntax PPrint Error
    ghci> import qualified Data.ByteString as BS
    ghci> errh <- initErrorHandle
    ghci> bs <- BS.readFile "inst/lib/Libraries/Arbiter.bo"
    ghci> (bi, bo, ipkg, hash) <- readBinFile errh "Arbiter.bo" bs
    ghci> putStr (ppReadable bi)

All 239 library modules are importable — parsers, type checker,
`ISyntax`/`ASyntax`, `.bo`/`.ba` (de)serialization, VCD handling, scheduling.
(`cabal repl bsc` also works but interprets the library from source — much
slower to load.)

## Caveats and invariants

* **Do not start `ghci` with cwd `src/comp`** — the legacy `src/comp/.ghci`
  (for the interpret-from-source workflow) is merged into the session and
  fights the package environment. Use the repo root, or `ghci -ignore-dot-ghci`.
* **The vendor solver libraries are a runtime dependency of every script.**
  The library declares `extra-libraries: stp yices` plus Tcl, so *any* script
  — even `dumpbo`, whose code never touches SAT — dlopens
  `libstp.so`/`libyices.so`/`libtcl8.6.so` at package load, from the absolute
  paths recorded at build time. Deleting/moving `src/vendor/{stp,yices}/lib`
  or relocating the checkout breaks the scripts until you re-run `setup.sh`.
  (Future work: split the SAT/Tcl cone into a private sub-library so utility
  scripts don't link solvers at all.)
* **Version stamping**: see the `setup.sh` note above.
* `getProgName` reports the script name (e.g. `dumpbo.hs`) in error/usage
  messages where the binaries report the tool name.
* The compiled binaries bake in RTS options (`-with-rtsopts=-H256m -K10m`);
  scripts run with runghc defaults. Pass `+RTS ... -RTS` explicitly if a
  large design needs it.
* `showrules`/`vcdcheck` consult `$BLUESPECDIR` (for `%` path expansion and
  default `.ba` search paths). The `bin/` launcher defaults it to
  `<repo>/inst/lib`; direct `runghc` invocations inherit your environment.

## Writing new scripts

Any Haskell file whose imports are bsc library modules works. Copy one of the
small scripts (`dumpba.hs` is 30 lines) as a starting point.

## Regenerating the module list

After adding/removing modules under `src/comp` (or the other source roots):

    python3 util/bluehs/gen-cabal-modules.py

(Candidate CI check: run this and fail if `bsc.cabal` changes.)

## Distribution

`util/bluehs/mk-dist.sh` builds `bsc-bluehs-<os>-<arch>-<version>.tar.gz`: a
self-contained, relocatable tree (pruned GHC runtime + relocatable package
store + SAT solver libraries + these scripts + a `bin/bluehs` launcher) so
tarball users can run Haskell scripts against the bsc library with **no
Haskell toolchain installed**. Host requirements: glibc, libgmp, libtcl8.6,
and a C compiler (GHC probes it when loading libraries; CPP scripts
preprocess with it).

This artifact is a *companion* to the main bsc tarball and must be built
from the same commit (the packaged library rejects `.ba` files whose build
version stamp differs — see Caveats). Ship both from one release action;
they are versioned in lockstep, like bluetcl.

Everything redistributed in the tarball is covered in its `LICENSES/`
directory: `LICENSE.ghc` (compiler/runtime), a generated
`LICENSE.ghc_pkgs` enumerating every shipped Haskell package with license
and copyright (via `src/comp/make-ghc-pkg-info.sh` over the exact shipped
package closure), and the STP/Yices texts for the bundled solver
libraries.

The scripts in this tarball provide what `make install-extra` builds as
compiled binaries; once bluehs ships as a standard release artifact,
`install-extra` is a candidate for retirement.

## Not converted

* `bsc` itself: works as a script mechanically, but the ~1.5s/invocation
  interpretation cost of the 2300-line driver is wrong for a tool invoked
  once per compilation unit. The right eventual shape is moving `hmain` into
  a library module (`Driver`), making `bsc` a 3-line compiled `Main` — after
  which custom driver *scripts* are trivial for those who want them.
* `bluetcl`: structurally not a script — it embeds Haskell in a C `main` via
  foreign exports and links libtcl/libhtcl.
