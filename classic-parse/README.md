# bsc-classic-parse

bsc's Bluespec Classic (Bluespec Haskell) lexer and parser, built on their
own as an accept/reject oracle.  `Lex.hs`, `Parser/Classic/CParser.hs` and
the modules they transitively import (48 in all) are compiled *unmodified*
straight out of `../src/comp`, so the oracle is always the parser of whatever
commit this tree is at.  Nothing here needs the compiler's C/C++ parts
(yices, stp, tcl) or its Setup hooks.

## Build

    nix develop .. -c cabal build     # or plain `cabal build` inside the dev shell
    cabal list-bin bsc-classic-parse  # where the binary went

A couple of minutes from cold.  The package dependencies are the same as
bsc's, so an already-populated cabal store is reused.

## Use

    bsc-classic-parse [--defns | --type] [--quiet] [--bsc-message] [FILE...]

Reads stdin when no FILE is given.  One line per input, in `bh-parse` style:

    NAME: ok
    NAME:LINE:COL: P0005 Unexpected "="; expected "\" or "interface"

Any bsc error tag can show up: lexical errors come out as P0001 etc., a
non-UTF-8 input as P0224 (at -1:-1, as in bsc).  `--bsc-message` prints
bsc's full multi-line text instead.

Exit status: 0 if everything was accepted, 1 if anything was rejected, 3 if
the parser itself crashed on an input (an internal error, or a stack overflow
under bsc's own `-K10m` limit; real bsc dies on such inputs too, rather than
reporting a syntax error), 2 for a usage error.

Modes: by default the whole input goes through `pPackage`, exactly what bsc
does with a `.bs` file (`Depend.parseSrc`); `--defns` parses a bare list of
top-level definitions (`pDefnsAndEOF`); `--type` a single type (`pType`, as
bluetcl does).  The bsc flags `-no-use-layout` and
`-outlaw-sv-kws-as-classic-ids` are accepted with their usual meaning, and
`$BSC_OPTIONS` is consulted as bsc does.

Throughput: about 1 ms per file when many files are passed to one
invocation, about 10 ms for a fresh process, so batch inputs when fuzzing.

## Differential testing

    ./difftest.sh FILE...
    find ../testsuite -name '*.bs' -print0 | ./difftest.sh -0

Runs the oracle and another parser (`bh-parse` on `$PATH`, or `-p PARSER` /
`$BH_PARSE`) over the same files and prints every file on which they
disagree about accept/reject, with both messages.  Exit 0 iff they agree
everywhere.  `--defns` is passed through to both.

## Caveats

* After a successful parse the whole AST is forced (via `show`), so an error
  that a semantic action raises lazily counts as a crash here; bsc itself
  would only hit it in a later pass.
* `difftest.sh` joins the two tools' output on the file name, so names
  containing `:` confuse it.
* `BuildVersion.hs` here is a stand-in for the module bsc's Setup hooks
  generate; it only feeds the version string in internal-error messages.
