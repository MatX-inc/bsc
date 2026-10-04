<div class="title-block" style="text-align: center;" align="center">

# Bluespec Compiler - Information for developers

---

</div>

Here you can find documentation on the internal architecture of [BSC](./README.md)
and other helpful information for people who want to contribute to the source code.

Feel free to ask questions on GitHub (in an Issue or a Discussion)
or on the [`bsc-dev`](https://groups.io/g/bsc-dev) mailing list.
The `bsc-dev` list is for questions that are only relevant to developers,
to keep traffic on the [`b-lang-discuss`](https://groups.io/g/b-lang-discuss)
mailing list light for people who are just users.

---

At the moment there is no formal documentation.
However, there are written responses to questions on GitHub and the mailing lists,
that can someday be collected and turned into a document.
The following is a running list of those writings.

### Basics / General info

* [BSC is a series of stages](https://groups.io/g/bsc-dev/message/14)
  * This write-up includes a link to the following (incomplete)
    [diagrams of the BSC stages](https://docs.google.com/document/d/1130fyOsPtS6gMppB6BaO-qVXxzO5b_ha7sXwLdd8Dtg/edit?usp=sharing)
  * See also [this brief breakdown of BSC](https://groups.io/g/b-lang-discuss/message/358)
    by its three internal representations (CSyntax, ISyntax, ASyntax)
  * Briefly on [printing and dumping from BSC and intermediate files](https://groups.io/g/b-lang-discuss/message/356)
* [More on the stages, the backend split, Bluesim stages, and the structure of Bluesim output](https://github.com/B-Lang-org/bsc/issues/743#issuecomment-2436483892)
* [The meaning of `.bo` and `.ba` files and compiler flow](https://github.com/B-Lang-org/bsc/discussions/575#discussioncomment-6458212)
* Hidden flags
  * BSC has a flag `-help-hidden` for developers,
    which shows more information than the `-help` for users
  * Like the LaTeX documentation for flags in the BSC User Guide,
    there is short LaTeX document for hidden flags at BS Inc (called `internal-user-guide`),
    which could become part of a BSC Developer Guide
* Names that BSC has built-in knowledge of (such as definitions in the Prelude)
  are specified in `src/comp/PreStrings.hs`, which are then wrapped as identifiers
  in `src/comp/PreIds.hs`
* For Haskell Language Server (HLS) setup see the dedicated [README file](./util/haskell-language-server/README.md)
* The [GitHub CI](./.github/workflows/) can serve as an example of building and working with the source.
  See, for example, [build-and-test-ubuntu.yml](./.github/workflows/build-and-test-ubuntu.yml) which contains steps for:
  * Installing dependencies
  * Running the Haskell Language Server (HLS)
  * Running GHC's interactive environment (GHCi)

### Compiling

* See [INSTALL.md](./INSTALL.md) for info on building and installing
* TBD: Any info on tools, dependencies, and compiling options
  * e.g. individual SMT libraries can be omitted using `STP_STUB=1` or `YICES_STUB=1`

### Testing

* See the test suite's own [README file](./testsuite/README.md)

### Formal verification

* [Discussion thread](https://github.com/B-Lang-org/bsc/discussions/333)
  * [This comment](https://github.com/B-Lang-org/bsc/discussions/333#discussioncomment-491981)
    mentions BSC stages for outputting modules in SAL or LambdaCalcus
    languages for formal analysis.  Similar stages for generating Lean
    or Rocq could be written using these as templates.

### Debugging

* [Example debugging rule condition evaluation](https://github.com/B-Lang-org/bsc/discussions/798#discussioncomment-14013205)
  * From the use of `-v` to identify stages, to using `-d` to dump
    stage outputs, to using `show-elab-progress` and `-trace-eval-nf`
    to debug evaluation

### BSC stage: Parsing

* [Keyword parsing in BH/Classic](https://github.com/B-Lang-org/language-bh/issues/5#issuecomment-1856814271)
* Various features in BSV are implemented implicitly with typeclasses,
  such as [updating and selecting with bracket syntax](https://groups.io/g/b-lang-discuss/message/877)
  and handling of uninitialized variables
* [Implicit reading and writing of registers](https://groups.io/g/b-lang-discuss/message/877)
  is handled in the type checker by looking for methods named `_read` and `_write`
* StmtFSM
  * [Library and the use of Position primitives](https://groups.io/g/bsc-dev/topic/81853144#msg21)
  * [Naming](https://github.com/B-Lang-org/bsc/pull/882#issuecomment-3887648972)

### BSC stage: Type checking

* [System tasks are handled specially](https://github.com/B-Lang-org/bsc/pull/780#discussion_r2045809939)
  * Because tasks can take variable argument lists, a dummy type
    signature is given in the Prelude
* See the link on the use of SMT solvers, below

### BSC stage: Elaboration

* [How to add a new evaluator primitive to BSC](https://groups.io/g/b-lang-discuss/message/526)
  * specifically how to add a function to get the current module name
* See the link on the use of SMT solvers, below

### BSC stage: Scheduling

* [Understanding scheduling](https://github.com/B-Lang-org/bsc/discussions/622#discussioncomment-7203579)
* See the link on the use of SMT solvers, below

### BSC backends / naming

* [Naming conventions in the generated Verilog](https://groups.io/g/b-lang-discuss/topic/106903347)
* [Verilog/Bluesim "main" and the naming of clock and reset ports](https://groups.io/g/b-lang-discuss/message/606)
* [System tasks/functions in ASyntax](https://github.com/B-Lang-org/bsc/pull/780#issuecomment-2811236368)

### BSC backend: Verilog

* [BSC's deduction of portprops](https://groups.io/g/b-lang-discuss/topic/106516831)
* [How to use the different Verilog directories (for different synth tools)](https://groups.io/g/b-lang-discuss/topic/106402322)

### BSC backend: Bluesim

* See the link on Bluesim stages, above, under Basics
* [How Bluesim works (mostly the VCD dumping)](https://github.com/B-Lang-org/bsc/issues/519#issuecomment-1873853532)
* [How Bluesim provides implementations for import-BVI](https://groups.io/g/b-lang-discuss/topic/106520424)
* [How the Bluesim C API is imported into Bluetcl](https://groups.io/g/b-lang-discuss/message/554)
* There is a template for making Bluesim standalone programs (without Tcl) in `bsc/util/bsim_standalone/`
* `-c` (codegen mode): per-module byte-identity and object reuse (see below)

#### `-c` (codegen mode): per-module byte-identity

Terminology: a module generated as a `-e` link's *top* is in "top form" (its
interface methods are fired by the design schedule that the link also
generates); everywhere else -- under `-c`, or as a submodule of any design --
it is in "block form".  Block form is the shared, reusable output; top form
is private to the link that made it.

`-sim -c M` emits `M`'s Bluesim C++ (and only `M`'s) without a runnable top.
Its output must equal the C++ it gets as a submodule when generated with the
same backend options; byte comparisons disable generated timestamps.
A link's own top is the exception: it is generated in "top form".
Both module pairs and legacy `.ba` inputs use the existing
version/timestamp/`codeGenOptionDescr` reuse check in `SimFileUtils.hs`.
For a pair, the generated header and object must be current relative to
both `.bmod` and `.bsched`.

Both forms use the same `M.bmod`/`M.bsched` pair. `.bmod` contains the
unscheduled module; `.bsched` binds to its payload hash and stores scheduling
results plus the compiled results of common materialization as an IR delta,
including any resulting schedule changes. The reader replays these results
instead of rerunning `aNoInline` or `aDropUndet` under the current flags.
The artifacts contain no invocation flags or `options` pragmas. The module
format remains `bsc-bmod-20261003-2`; the schedule format is
`bsc-bsched-20261003-5`. Rescheduling with different options writes a new
`.bsched` while leaving `.bmod` unchanged.

Three possible sources of differences between block-form emissions need
care:

* **Interned-`Id` order** varies with what else is compiled, but only *reorders*
  output, and every per-module emission site is name-sorted (the four
  "Canonicalize ... order" commits, plus pre-existing sorts).
* **The schedule** (`M`'s own, firing-suppressed, vs the whole design's) reaches
  per-module codegen through three channels, each made `M`-intrinsic:
  *top-ness* (`mkScheduleStmts` drops `M`'s own interface-method firing, so DCE
  gives submodule form), *VCD clock annotation* (`clk_map`; the emitted domain
  comes from the module pair), and *member-vs-local* (`moveDefsOntoStack`; a top
  interface method's readiness is reached by the RDY call, as a parent would,
  not a direct read).
* **Codegen-time flags** come from the current invocation and must match for
  byte-identical output. For example, `-keep-fires` affects generated members,
  and `-unspecified-to` controls ASAny values that remain for
  `SimPackageOpt`/`SimBlocksToC`. Saved results of common materialization are
  already fixed in the pair. Any future reuse key must cover the backend's
  remaining inputs.

`check_block_codegen_modules` (`testsuite/config/unix.exp`) enforces it: it
rebuilds every multi-module test's submodules with `-c` and byte-compares.

The Verilog backend has the same mode: `-verilog -c M` regenerates `M.v` from
the module pair (via `vGenMods` in `bsc.hs`). To match the direct compile,
pass the same effective backend options, including those originally supplied
by `(* options *)` pragmas. Reading the pair preserves the earlier scheduling
and common materialization choices; the new invocation controls Verilog
generation. Foreign-function `.bdpi` files are found via the same
`-p`/`-bdir` search path as at link time, with legacy foreign `.ba` fallback.
Verilog linking reuses a `.v` that is current relative to both pair members;
legacy inputs use the single `.ba` timestamp. This check has no backend-option
descriptor, so use `-c` to regenerate after changing backend options.

With `-stable-verilog` (default on), backend choices that used to lean on
`Id`'s `Ord` (the SpeedyString intern order, which varies with compile history) use
the identifier's text -- topological-sort tie-breaks, CSE survivor names,
port/mux/gate orderings -- and a final `VStableRenumber` pass renumbers the
compiler-minted name families (`__d`/`__h`/`__q`/`__f`/`_dm`/`_ds`) into
first-use order (foreign linkage names are excluded: a "BDPI"-imported
`f__h1` must keep its C symbol).  The testsuite enforces the contract at
the point of generation: since a `-verilog` compile writes the module pair by
default, `check_verilog_regen` (called from `bsc_compile_verilog` in
`testsuite/config/unix.exp`, so every Verilog-compile proc gets it)
regenerates each `.v` the compile just produced from its pair with `-c`
and requires a byte-identical result, skipping invocations whose flags
make the comparison meaningless (`-no-stable-verilog`, `-elab-only`/
`-no-elab`, relocated outputs, `-verilog-filter`).  The regeneration runs
in a fresh bsc process, which is the point: in the process that wrote the
`.v` every string is already interned, so a same-process regen would see
the same interning history and could not fail for interning-order
reasons. `testsuite/bsc.verilog/stable_verilog/` holds targeted trigger
designs and compares direct and regenerated output under matching options,
including explicit `-no-stable-verilog` cases.

The mirror image is `-elab-only`, which makes a `-verilog` compile stop at
the module pair, exactly as a Bluesim compile does: `genModuleVerilog` is not run
at all, and every module's `.v` comes from `-c` or the link.  The flag is
backend-agnostic when compiling source -- with `-sim` (or no backend)
stopping at the pair is already the behavior, so it is accepted as a
no-op, letting build systems pass it unconditionally.  The wrapper's port
properties come from the APackage analysis (`getIOPropsA`) before the
backend split and are recorded in the `.bo` under `-elab-only` too, so a
parent compiled against an `-elab-only` child deduces exactly what a full
compile deduces -- the staged flow has no annotation caveat.

### Bluetcl

* [Support for reflection in BSC](https://groups.io/g/b-lang-discuss/message/513)
  * specifically, Bluetcl (outside the language) and Generics (inside the language)
* See the link on how Bluesim's C API is imported into Bluetcl, above, under Bluesim
* [Extracting source type information for display in a waveform viewer](https://groups.io/g/b-lang-discuss/topic/116530680#msg887)

### SMT solvers

* [The ways that SMT solvers are used in BSC](https://groups.io/g/b-lang-discuss/message/370)
* [SAT solver usage and dumping](https://github.com/B-Lang-org/bsc/discussions/693#discussioncomment-9148985)
* TBD: Status of the SMT solver source codes and how they are incorporated into BSC
* [How a new feature might be added to specify assumptions for the SMT solver](https://github.com/B-Lang-org/bsc/discussions/417#discussioncomment-1505426)
  * From handling in the evaluator to ISyntax to ASyntax to scheduling

### Clock and Reset methodology

* [Clock/reset inference](https://github.com/B-Lang-org/bsc/discussions/661)
* BSC implements certain design decisions for clocks and resets --
  for example, the choice to implement reset inside of state elements (to ignore the `EN` input)
  instead of outside (as part of the `RDY` logic) --
  and there may be some documentation (perhaps internal to BS Inc) on those decisions
  * There was a paper at MEMOCODE 2006,
    ["Reliable Design with Multiple Clock Domains"](https://www.researchgate.net/publication/224648422_Reliable_design_with_multiple_clock_domains)
    * An earlier version of this paper was submitted to DCC'06 (Designing Correct Circuits)
  * There is a BS Inc document from 16 Dec 2004 (`mcd.pdf`) that discusses some options, but only clocks, not yet reset
  * There is a BS Inc document from 28 Oct 2004 (`resets.txt`) that purports to be "a proposal on reset handling"
    but is very prelimary about the problem, not yet the solution
  * There is a BS Inc file `bsc-doc/doc/MCD-extensions.txt` that describes
    the new things in BSC to support MCD, both user visible attributes and
    the BSC source code changes
  * The BS Inc training slides include a
    [lecture on MCD](https://github.com/BSVLang/Main/blob/master/Tutorials/BSV_Training/Reference/Lec12_Multiple_Clock_Domains.pdf)

---
