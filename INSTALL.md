# Compiling BSC from source

Source code for the Bluespec toolchain can currently be built on Linux
and macOS. It may compile for other flavors of Unix, but likely will need
additional if/else blocks in source code or Makefiles.

The core of BSC is written in Haskell, with some libraries in C/C++.

---

## Overview

The following sections describe the requirements and commands for building
BSC.  Running the build commands will result in the creation of a directory
(named `inst` by default) that contains an _installation_ of BSC.  This
directory can be moved to anywhere on your system, but it is best for the
files to remain in their relative positions within the directory.

We recommend renaming the `inst` directory to `bsc-${BSC_VERSION}` and placing
it in a subdirectory of `/opt/`, `${HOME}/`, `/usr/share/`, or similar
location.  For example:

```bash
BSC_VERSION=$(echo 'puts [lindex [Bluetcl::version] 0]' | inst/bin/bluetcl)
mkdir -p /opt/tools/bsc
mv inst /opt/tools/bsc/bsc-${BSC_VERSION}
cd /opt/tools/bsc
ln -s bsc-${BSC_VERSION} latest
```

The `inst` directory has a `bin` subdirectory, where the executables
for the tools are found.  To use the tools, just add that directory to
your `PATH`:

```bash
export PATH=/opt/tools/bsc/latest/bin:$PATH
```

These executables will make use of other files found within the `inst`
directory, locating them relatively from the `bin` directory.  That is
why the directory must be kept together.

If you are packaging BSC for an OS (for example, into a `.deb` or `.rpm`
file), your package can't simply move the `bin` files to `/usr/bin/`
and the `lib` files to `/usr/lib/` and so on.  We recommend placing the
`inst` directory at `/usr/share/bsc/bsc-${BSC_VERSION}` and then creating
symlinks in `/usr/bin/` that point to the executables in
`/usr/share/bsc/bsc-${BSC_VERSION}/bin/`.

---

## Requirements

To build a complete release of BSC, you will need:
 - The standard Haskell compiler [GHC]. The recommended version is
   9.6.7, which is the version built and tested by this project's
   continuous integration (CI).  Other versions are untested and may
   not work.  We recommend installing GHC via the popular installer
   [GHCup].
 - The Haskell build tool `cabal-install` (the `cabal` command),
   version 3.14 or newer (the BSC package uses Cabal's `Hooks` build
   type, which older versions do not support).  We recommend
   installing it via [GHCup] as well.  The additional Haskell library
   dependencies are downloaded from [Hackage] and built by `cabal`
   itself, so an internet connection is needed (at least for the
   first build).
 - The GNU Multiple Precision Arithmetic Library (GMP). `libgmp` is
   used to implement integers in Haskell and may already be a
   dependency of installing GHC.
 - `pkg-config` is strongly recommended to query installed
   libraries. The build will fall back to default values if necessary,
   but this should be avoided if possible.
 - Standard unix shell and development tools, notably GNU Make.

The following dependencies are optional, though recommended:
 - To build the Yices SMT solver: a C/C++ toolchain, `autoconf` and
   the `gperf` perfect hashing library.
 - To build the STP SMT solver: a C/C++ toolchain, Perl, and the
   `flex` and `bison` parser generator tools.
 - To build the Bluespec Tcl shell (`bluetcl`): Tcl development
   libraries (version 9.0, 8.6, or 8.5).
 - To run smoke tests: the [Icarus Verilog] simulator.
 - To run the full test suite: the Icarus Verilog simulator, Perl,
   csh, and SystemC libraries. See the [testsuite
   README](testsuite/README.md) for details.
 - To build PDF documentation: a LaTeX installation, with extras and
   additional fonts.
 - To format release notes for publication, the [Asciidoctor] tool.

[CI workflow]: .github/workflows/ci.yml
[GHC]: https://www.haskell.org/ghc/
[GHCUp]: https://www.haskell.org/ghcup/
[Hackage]: https://hackage.haskell.org
[Icarus Verilog]: https://steveicarus.github.io/iverilog/
[Asciidoctor]: https://asciidoctor.org

### Debian and Ubuntu systems

The following commands install all required and optional dependencies:

```bash
sudo apt-get install \
   build-essential \
   tcl-dev \
   libgmp-dev \
   pkg-config \
   autoconf \
   gperf \
   flex \
   bison \
   iverilog \
   texlive-latex-base \
   texlive-latex-recommended \
   texlive-latex-extra \
   texlive-font-utils \
   texlive-fonts-extra

curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ghcup install ghc 9.6.7
ghcup install cabal latest
cabal update
```

Those final commands install the recommended GHC compiler version and
the `cabal` build tool, and download the [Hackage] package index that
`cabal` uses to resolve the Haskell library dependencies.  Note well
that the version of `ghc` tested for building the Bluespec toolchain
is the version specified in the `ghcup install ghc` command above, and
that `cabal` must be version 3.14 or newer; system package managers
often provide older versions of both.

### Fedora systems

The following commands install all required and optional dependencies:

```bash
sudo dnf install \
   @development-tools \
   @c-development \
   iverilog \
   dejagnu \
   tcl-devel \
   gmp-devel \
   gperf \
   latex \
   texlive-boxedminipage \
   texlive-dingbat \
   texlive-fancybox \
   texlive-moreverb \
   texlive-scheme-basic \
   texlive-subfigure

curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ghcup install ghc 9.6.7
ghcup install cabal latest
cabal update
```

Those final commands install the recommended GHC compiler version and
the `cabal` build tool, and download the [Hackage] package index that
`cabal` uses to resolve the Haskell library dependencies.  Note well
that the version of `ghc` tested for building the Bluespec toolchain
is the version specified in the `ghcup install ghc` command above, and
that `cabal` must be version 3.14 or newer; system package managers
often provide older versions of both.

### MacOS systems

BSC builds on MacOS running on newer Apple (arm64) chips and older
Intel (x86_64) chips.  We test on MacOS 13 (x86_64), MacOS 14 (arm64),
and MacOS 15 (arm64).  To build on MacOS, you need the Command Line
Tools from Apple's [Xcode] app.  With Xcode installed, the following
command will install the Command Line Tools:

```bash
xcode-select --install
```

[XCode]: https://apps.apple.com/us/app/xcode/id497799835

We use the [Homebrew] package manager to install dependencies.  After
the [Xcode] Command Line Tools are installed, [Homebrew] can be
installed with the following command from their website:

```bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

[Homebrew]: https://brew.sh

After [Homebrew] is installed, the following commands install all
required and optional dependencies:

```bash
brew update
brew install \
   autoconf \
   gmp \
   gperf \
   icarus-verilog \
   pkg-config \
   deja-gnu \
   systemc \
   asciidoctor \
   texlive

curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ghcup install ghc 9.6.7
ghcup install cabal latest
cabal update
```

Those final commands install the recommended GHC compiler version and
the `cabal` build tool, and download the [Hackage] package index that
`cabal` uses to resolve the Haskell library dependencies.

### GHC Haskell compiler

As shown in the above summaries, we recommend installing GHC via the
popular installer [GHCup], which is available for Linux, FreeBSD,
macOS, and WSL2 on Windows.  This allows easily installing the
recommended version of GHC. The package manager for your OS may
provide a package for GHC, however it may be for a different version
of GHC.  The Haskell dependency bounds declared in `bsc.cabal`
currently target the recommended version (the one built and tested by
CI); the source code itself carries compatibility macros for a wider
range of GHC versions, but building with a different GHC may require
adjusting those bounds and is untested.

### Haskell libraries via Cabal

Building BSC requires some additional Haskell libraries beyond the
ones that ship with GHC.  You do not need to install these yourself:
they are declared as dependencies in `bsc.cabal`, and `cabal`
downloads and builds them as part of building BSC.  All that is needed
is a `cabal` new enough for the package (version 3.14 or newer, for
the `Hooks` build type) and a Hackage package index (`cabal update`).

To build a version of BSC that supports profiling, the dependencies
must also be built for profiling; this happens automatically when
passing `--enable-profiling` to `cabal` (see "Build the BSC
toolchain" below).

### SMT solvers

The repository for the [Yices SMT Solver] is cloned as a submodule of
this repository. Building the BSC tools will recurse into this
directory and build the Yices library, which is statically linked
into BSC and Bluetcl.

[Yices SMT Solver]: https://github.com/SRI-CSL/yices2

Building the BSC tools will also recurse into a directory for the STP
SMT solver. This is currently an old snapshot of the STP source code,
including the code for various libraries that it uses. In the future,
this may be replaced with a submodule instantiation of the repository
for the [STP SMT solver]. When that happens, additional requirements
from that repository will be added.

[STP SMT solver]: https://github.com/stp/stp

Both solvers are built (by `cabal`, through hooks that invoke the
vendored Makefiles) and statically linked into the tools, so the
installation does not carry separate solver library files.  To skip
building one of the solvers, see "Optionally avoiding the compile of
STP or Yices" below.

## Clone the repository

Clone this repository by running:

```bash
git clone --recursive https://github.com/B-Lang-org/bsc
```

That will clone this repository and all of the submodules that it depends on.
If you have cloned the repository without the `--recursive` flag, you can setup
the submodules later with a separate command:

```bash
git clone https://github.com/B-Lang-org/bsc
git submodule update --init --recursive
```

## Build the BSC toolchain

At the root of the repository:

```bash
make install-src
```

This will create a directory called `inst` containing an installation of the
compiler toolchain. This `inst` directory can later be moved to another
location; the tools do not hard-code the install location.

Under the hood, the Haskell tools (and the SMT solvers statically
linked into them) are built with `cabal`, and the rest of the
installation (the Bluespec standard library, the Bluesim kernel, the
Verilog primitives, and so on) is built by the Makefiles under `src/`.
Developers can also invoke `cabal build`, `cabal repl`, and
`cabal test` directly; see [DEVELOP.md](DEVELOP.md).

If you wish, you can install into another location by assigning the variable
`PREFIX` in the environment:

```bash
make PREFIX=/opt/tools/bsc/bsc-${BSC_VERSION} install-src
```

However, note that the `full_clean` target will delete the `PREFIX`
directory!

The default install includes the `bsc` and `bluetcl` binaries; the
extra utilities (`bsc2bsv`, `bsv2bsc`, `dumpbo`, `dumpba`,
`showrules`, and `vcdcheck`) can additionally be installed with:

```bash
make install-extra
```

Options can be passed to the underlying `cabal build` by defining
`CABAL_BUILD_FLAGS`; for example, for an unoptimized or a profiling
build of the Haskell code:

```bash
make CABAL_BUILD_FLAGS="--disable-optimization" install-src
make CABAL_BUILD_FLAGS="--enable-profiling" install-src
```

You can provide the `-j` flag to `make` to specify the number of targets
to execute in parallel; `cabal` similarly parallelizes its work across
packages and modules.  To additionally specify the number of modules
that GHC may compile in parallel, define `GHCJOBS` in the environment
to that number:

```bash
make GHCJOBS=4 install-src
```

The RTS options of the GHC processes doing the compilation (heap
sizes, for instance) can similarly be set via `GHCRTSFLAGS`, e.g.
`GHCRTSFLAGS='+RTS -M5G -A128m -RTS'`.

### Optionally avoiding the compile of STP or Yices

The BSC tools need an SMT solver. By default, the build process
compiles both the Yices and STP solvers, and allows the end user to
select which one to use at runtime, with Yices being the default.

Most users will never need to switch solvers, or even be aware of the
option. Thus, the build process offers the option of not compiling one
of the two solvers.

The solvers are statically linked into the BSC executables.  BSC
calls a function in the library to query its version; the way to omit
a solver is therefore to replace it with a stub that returns a null
version, which makes BSC refuse to select that solver at runtime.

To skip building the STP solver, assign a non-empty value to
`STP_STUB` in the environment:

```bash
STP_STUB=1 make install-src
```

Similarly, use `YICES_STUB` to skip building the Yices solver:

```bash
YICES_STUB=1 make install-src
```

The BSC tools do need at least one SMT solver, so only one of these
options should be used.  Note that the choice is baked in when the
solver libraries are first built; when toggling it, clean first
(`make clean`).

## Test the BSC toolchain

The following command will run a smoke test to ensure the compiler and
simulator work properly:

```bash
make check-smoke
```

The test suite can also be run through `cabal` (after `make
install-src` has built the runtime): `cabal test smoke` for a quick
spread, or `cabal test testsuite` for the whole suite.

For more extensive testing, see the
[testsuite README](testsuite/README.md) in the `testsuite`
subdirectory.

### Choosing a Verilog simulator

By default, the smoke test uses [Icarus Verilog] to test the Verilog code generation.
The Makefile in `examples/smoke_test` shows how you can point the default
`check-smoke` target at other Verilog simulators such as [Verilator],
VCS and VCSI (Synopys), NC-Verilog & NCsim (Cadence), ModelSim (Mentor), and CVC.

[Verilator]: https://www.veripool.org/wiki/verilator

## Build documentation

To build and install the PDF documentation, you can add the following:

```bash
make install-doc
```

This will install into the same `inst` or `PREFIX` directory.
The installed documents include the [BSC User Guide]
and the [BSC Libraries Reference Guide].

[BSC User Guide]: https://github.com/B-Lang-org/bsc/releases/latest/download/bsc_user_guide.pdf
[BSC Libraries Reference Guide]: https://github.com/B-Lang-org/bsc/releases/latest/download/bsc_libraries_ref_guide.pdf

## Building a release

The Makefile provides a single target, `release`, that will perform the above
steps (of building the tools and the docs) and will also install a few
additional files, creating a complete release in the `inst` directory:

```bash
make release
```

The additional files include a README, copyright and licensing info,
and release notes.  The release notes are written in
[AsciiDoc](https://asciidoc.org/) format that is published to HTML and
PDF format using the [Asciidoctor] tool, which is therefore a
requirement for building a release.

If you do not have Asciidoctor or would prefer not to install it (and all of
its dependencies), you can set `NOASCIIDOCTOR` in the environment:

```bash
make NOASCIIDOCTOR=1 release
```

This will install the raw AsciiDoc release notes, but will not install
the HTML and PDF versions.

## Exporting the source code

If you wish to make a snapshot of the source code available, outside
of Git, you can do so with `git archive`, but be aware of two points.

For one, you will need to also export the files from submodules,
because Git will not include them.

For two, you may wish to give a particular version name to
installations built from the snapshot.  The build uses Git to
automatically generate the version information for the compiler and
place it in the file `src/comp/BuildVersion.hs`.  The script that
generates this, `src/comp/update-build-version.sh`, can only query Git
for version info when called from inside a Git repository.  The
script will still work if `git archive` is used to export the
snapshot, because we have specified (in `.gitattributes`) that
patterns in the file should be substituted with their values (the
commit hash and tag, if any) during export.  Therefore, no change is
required.  However, if you want to hard-code a different version
name, you can pre-generate the `BuildVersion.hs` file and prevent the
build from regenerating it by setting `NOUPDATEBUILDVERSION=1` in the
environment.

---

## Using the Bluespec compiler

The installation contains a `bin` directory. To run the BSC tools, you only
need to add the `bin` directory to your path (or provide that path on the
command line). The executables in that directory will expect to find other
files in sibling directories within that same parent installation directory. If
you just built the compiler, you can quickly test it like so:

```bash
export PATH=$(pwd)/inst/bin:$PATH
```

> **NOTE**: Earlier versions of BSC required that the environment variable
> `BLUESPECDIR` be set to point into the installation directory; this is no
> longer necessary, as the executables will figure out their location and
> determine the installation location on their own.

Run the following to see command-line options on the executable:

```bash
bsc -help
```

Additional flags of use to developers can be displayed with the
following command:

```bash
bsc -help-hidden
```

More details on using BSC, Bluesim, and Bluetcl can be found in the
[BSC User Guide] (built in this repository).
For language documentation and learning materials, see the
[Documentation section of the README](./README.md#documentation).

## Editors

Support for various editors for BH/BSV sources as well as language
server support for the haskell sources for the bluespec compiler can
be found in the [./util](./util) directory.
