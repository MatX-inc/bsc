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
   9.6.7, which is the primary version built and tested by this
   project's continuous integration (CI); newer versions are also
   tested (see the [CI workflow] for the exact set).  Older versions
   are untested and may not work.  We recommend installing GHC via
   the popular installer [GHCup].
 - The Haskell build tool [cabal-install], version 3.16, which fetches
   the Haskell libraries the build needs beyond GHC's own from Hackage.
   We recommend installing it via [GHCup] as well.
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
[cabal-install]: https://www.haskell.org/cabal/
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
ghcup install cabal 3.16.1.0
cabal update
```

Those final four commands install the recommended GHC compiler version
and the cabal build tool, and fetch the package index the build reads. If you would prefer to install GHC via the system's
package manager, which may install an older version, you can
substitute the following command.  Note well that the version of `ghc`
tested for building the Bluespec toolchain is the version specified in
the `ghcup install ghc` command above.  If your system packages an
older `ghc` (check with `ghc --version`), it might not work.

```bash
sudo apt-get install \
   ghc \
   cabal-install
```

If you use the system's package manager and want to profile the
Bluespec compiler (unlikely for most users), you will also need the
profiling-enabled versions of GHC's own libraries:

```bash
sudo apt-get install ghc-prof
```

The system's `cabal-install` may be older than the 3.16 the build needs
(check with `cabal --version`); `ghcup install cabal 3.16.1.0` gives
that one.

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
ghcup install cabal 3.16.1.0
cabal update
```

Those final four commands install the recommended GHC compiler version
and the cabal build tool, and fetch the package index the build reads. If you would prefer to install GHC via the system's
package manager, which may install an older version, you can
substitute the following command.  Note well that the version of `ghc`
tested for building the Bluespec toolchain is the version specified in
the `ghcup install ghc` command above.  If your system packages an
older `ghc` (check with `ghc --version`), it might not work.

```bash
sudo dnf install \
   ghc \
   cabal-install
```

If you use the system's package manager and want to profile the
Bluespec compiler (unlikely for most users), you will also need the
profiling-enabled versions of GHC's own libraries:

```bash
sudo dnf install ghc-prof
```

The system's `cabal-install` may be older than the 3.16 the build needs
(check with `cabal --version`); `ghcup install cabal 3.16.1.0` gives
that one.

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
ghcup install cabal 3.16.1.0
cabal update
```

Those final four commands install the recommended GHC compiler version
and the cabal build tool, and fetch the package index the build reads.

### GHC Haskell compiler

As shown in the above summaries, we recommend installing GHC via the
popular installer [GHCup], which is available for Linux, FreeBSD,
macOS, and WSL2 on Windows.  This allows easily installing the
recommended version of GHC. The package manager for your OS may
provide a package for GHC, however it may be for an older version of
GHC.  This is fine, as the BSC source code has been written with
extensive preprocessor macros to support every minor release of GHC
since 7.10, through 9.12. Any releases in that range should be fine;
however, newer versions may be more efficient.
GHC releases older than 7.10.3 are not supported.

BSC releases are tested and built with the recommended stable version
of GHC, currently 9.6.7.  The source is also tested with a range of
other GHC versions (currently 8.8.4 through 9.4, and 9.8 through 9.14)
any of which are also fine.

### Haskell libraries via Cabal

The compiler is built by `cabal`, which `make install-src` runs (see
`doc/cabal.md`), and the Haskell libraries it needs beyond GHC's own
are fetched from Hackage into cabal's own store under your home
directory: nothing is installed into the GHC installation, and no
library needs installing by hand.  Use cabal-install 3.16, the version
the build's hooks need and the one whose GHC job server protocol GHC
9.8 through 9.14.1 speak (a 3.18 falls back to a less efficient
parallel build with those).  As with GHC, we recommend installing
`cabal` via `ghcup`:

```bash
ghcup install cabal 3.16.1.0
cabal update
```

The second command fetches the Hackage package index, which the build
reads.

To build a version of BSC that supports profiling, be aware that
profiling versions of the libraries need to be present: cabal builds
those of the libraries it fetches when profiling is enabled
(`profiling: True` in `cabal.project.local`), and GHC's own come with
GHC (the `ghc-prof` package, where the OS packages GHC).

### SMT solvers

The repository for the [Yices SMT Solver] is cloned as a submodule of
this repository. Building the BSC tools will recurse into this
directory and build the Yices library for linking into BSC and
Bluetcl.

[Yices SMT Solver]: https://github.com/SRI-CSL/yices2

Building the BSC tools will also recurse into a directory for the STP
SMT solver. This is currently an old snapshot of the STP source code,
including the code for various libraries that it uses. In the future,
this may be replaced with a submodule instantiation of the repository
for the [STP SMT solver]. When that happens, additional requirements
from that repository will be added.

[STP SMT solver]: https://github.com/stp/stp

Both the Yices and STP solvers are optional to build, although
recommended. To skip these builds, see "Optionally avoiding the
compile of STP or Yices" below.

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

If you wish, you can install into another location by assigning the variable
`PREFIX` in the environment:

```bash
make PREFIX=/opt/tools/bsc/bsc-${BSC_VERSION}
```

However, note that the `clean` target will delete the `PREFIX` directory!

The compiler is one cabal package (see `doc/cabal.md`), and a variant of
its build is a `cabal.project.local` at the root of the repository, which
`make install-src` honors and git ignores.  Unoptimized:

```
optimization: False
```

Debug, with Core lint and the debugging runtime:

```
package bsc
  ghc-options: -dcore-lint -debug
```

Profiling, with cost centres on every function and the larger stack cap
their frames need:

```
profiling: True
profiling-detail: all-functions
package bsc
  ghc-options: "-with-rtsopts=-H256m -K256m -i1"
```

You can provide the `-j` flag to `make` to specify the number of targets
to execute in parallel, however this does not control the parallelism of
the core haskell build.  To specify the number of modules that GHC may
compile in parallel, define `GHCJOBS` in the environment to that number:

```bash
make GHCJOBS=4
```

### Optionally avoiding the compile of STP or Yices

The BSC tools need an SMT solver. By default, the build process
compiles both the Yices and STP solvers, and allows the end user to
select which one to use at runtime, with Yices being the default.

Most users will never need to switch solvers, or even be aware of the
option. Thus, the build process offers the option of not compiling one
of the two solvers.

Currently, the BSC executable expects to dynamically link with
object files for Yices and STP found in the directory `inst/lib/SAT/`.
BSC calls a function in the library to query its version; if the version
does not match what BSC expects, BSC will not let users select that solvers.
Thus, the current way to omit a solver is to replace the object file
with a stub that returns a null version.  In the future, we may replace
this with static linking and the processing for removing a solver 
would then simply omit the code for that solver.

To skip building the STP solver, assign a non-empty value to
`STP_STUB`:

```bash
make STP_STUB=1
```

Similarly, use `YICES_STUB` to skip building the Yices solver:

```bash
make YICES_STUB=1
```

The BSC tools do need at least one SMT solver, so only one of these
options should be used.

## Test the BSC toolchain

The following command will run a smoke test to ensure the compiler and
simulator work properly:

```bash
make check-smoke
```

For more extensive testing, see the [testsuite README]
in the `testsuite` subdirectory.

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

For two, the version information the compiler and the Bluesim kernel
carry is generated from Git by `src/comp/update-build-version.sh`, which
the build runs, and the script can only query Git when called from
inside a Git repository.  It will still work if `git archive` is used
to export the snapshot, because we have specified (in `.gitattributes`)
that patterns in the script should be substituted with their values
(the commit hash and tag, if any) during export.  Therefore, no change
is required.  Without Git and without that substitution, set `NOGIT=1`
in the environment for a null version.

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
