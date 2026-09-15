# trs-bir

Exports a Bluespec design's post-schedule IR as BIR.

It reads the design's `.ba` hierarchy, expands it into a `SimSystem`,
optimizes the packages, and serializes the result:

    trs-bir sysTop
    trs-bir -p build -o top.bir sysTop

The output is `<top>.bir` in the working directory, or wherever `-o`
says.  The rest of the command line is a search path for `.ba` files and
`--keep-fires`; those are the settings the export reads, and there are
no others.  Parsing is `System.Console.GetOpt` from `base`, so the tool
needs no package this build does not already have.

## How it is built

The semantics of the export come from reuse rather than a second
implementation: `simExpand`, `simPackageOpt`, and `SimExportIR` are the
compiler's own modules, reached on the GHC search path.  `shadow/` sits
ahead of `../../comp` on that path and holds any module `trs-bir` needs
to behave differently; it is empty.

The closure is a fraction of bsc's: elaboration, type checking, and
every code generator sit downstream of the `.ba` this program reads, so
none of them are built.

No SAT solver is in that closure and neither vendored solver is built or
linked: `AOptCase` holds the two case-building transformations the
Bluesim path takes from `AOpt`, and `AOpt` is what reaches a solver.
The binary depends on nothing outside the standard libraries.

## Installing

`make install` puts the binary in `bin/core/trs-bir` and the compiler's
`wrapper.sh` in `bin/trs-bir`, the same shape bsc installs in.  The
wrapper supplies `BLUESPECDIR`, which is where the `.ba` search path
ends.  With that set in the environment, or with the `.ba` files in the
working directory, the binary also runs directly.
