# bsc recabalization brief, Revision 2: packages, per-component Warmup, the fingerprint carve, and a Flags re-architecture

Revision 2, 2026-10-01. Base: MatX-inc/bsc release-devel-B0 @ 9306c345.
Author: Claude (gate session on Ravi's PVM). Supersedes Revision 1 plus
Addendum 1 (2026-09-30, commits 9f4ceb5b and 96f13139), after Codex's
adversarial review of 2026-09-30 (fifteen findings; the review text is in
the KB draft "KB: REVIEW REQUEST — bsc recabalization + Flags
re-architecture brief (B0)"; Appendix D answers each finding). Status:
specification for the amended step-0 experiment Codex's verdict
conditionally endorsed. Ravi's standing instructions for the experiment:
GHC 9.14, bit-identical objects, and a Warmup that actually takes effect in
every component; performance measurement is handled by Ravi separately and
is not a gate here.

## 0. What changed since Revision 1

- The partition is corrected from driver call sites, not import edges
  alone: the .bo producer runs IConv, IConvLet, GroundCType, LiftDicts,
  ISimpDicts and ISimplify before genBinFile, so they are bsc-typecheck;
  aRenameIO, aDropDefs and aSynthesize are called only from the Verilog
  path, so they are bsc-verilog; AOpt and AExpand are the ASyntax optimizer
  both backends run (Bluesim through aOptAPackageLite, aExpandDynSel and
  aInsertCaseDef), so they form bsc-aopt; ACheck, Params and ISyntaxCheck
  are placed by their own imports (core, core, elab). Ten components plus
  setup and facade. Zero edges outside the DAG (Appendix A).
- Warmup gets a contract (section 3.4): one hidden module named Warmup per
  package, generated from direct dependencies with the public compatibility
  Warmup excluded, and an import edge from every component root, 66
  one-line source additions the generator verifies. The facade package is
  Hooks too, so the executables get the same treatment.
- No hook writes into src/comp. The make build generates its own Warmup
  and BuildVersion as it does today.
- Package invalidation and action identity are kept apart (section 3.6).
  The experiment makes no selective-reuse claim; cache keys stay
  whole-execution until producer closures are audited.
- Determinism is a gate with a definition (G1''), not an assertion.
- Flags and the .ba envelope move off the critical path (sections 4 and 5)
  and carry Codex's corrections: effective options are per action closure,
  the Verilog link path's reuse check needs a descriptor, pragma timing is
  observable, the codec takes neutral metadata.
- Sequencing follows the plan of record: the experiment does not gate the
  P1 and P2 library win.
- Evidence is checked in (doc/recabalization-evidence) and labeled;
  the scratch copies and raw logs of 2026-09-30 were lost to a reboot.

## 1. Verified facts

Labels: [source] independently checkable in the pinned tree or the pinned
cabal/GHC sources; [toy] reproduced on this machine with the checked-in toy;
[reported] measured here once, raw log lost.

F1 [source]. cabal-install 3.16.1.0 builds a Hooks package as one unit and
rejects sublibraries in it: "Internal libraries only supported with
per-component builds. Per-component builds were disabled because build-type
is Hooks." ProjectPlanning.hs: "Custom and Hooks are not implemented"; the
`per-component` project setting can only disable the mode. Consequence: no
sublibrary carve of any granularity can live inside a Hooks package.

F2 [source]. BuildSystem, BuildVersion and Warmup are imported by core leaf
modules (Bag, Classic, Changed, EquivalenceClass, BDD, GraphPaths,
Exceptions, Fixity, SEMonad, RealUtil, Log2, Version, STPFFI, YicesFFI) and
by SimBlocksToC and the executables. Warmup must be a home module of the
component it serves (F7); BuildSystem and BuildVersion are ordinary
imports and could live anywhere below their importers.

F3 [toy, source]. GHC's finder resolves an unqualified import from the home
unit's source path before it consults packages. Two sublibraries listing
one shared directory each compiled a private copy of the module they
imported from a dependency; -Wmissing-home-modules only warned; the
executable failed with "Couldn't match expected type 'pkgb-0.1:b-y:M1.T'
with actual type 'T' ... defined in package 'pkga-0.1'". Consequence: each
component needs a disjoint source directory, and a cross-DAG import then
fails with "Could not find module", which is the DAG check at build time.
Toy: doc/recabalization-evidence/shadowtest.

F4 [source]. `hs-source-dirs: ../dir` is the `relative-path-outside`
check warning at build time. Moot for the layout in 3.3, where no path
leaves its package.

F5 [toy]. A dependency library's `ld-options` reach an executable in
another package through the package database (RUNPATH observed on the
toy's executable). Only bsc-core needs the solver rpath hook. Linux only;
macOS and ghci/runghc loading are not covered by the toy.

F6 [toy]. A Hooks package's `setup-depends` may name a local library in
the same project, and a hook generates a per-package Warmup from Cabal's
installed package index using the component's direct dependencies
(componentPackageDeps looked up in installedPkgs), in-place ones included;
the dependent toy package's Warmup contained `import CoreM ()`. The shared
hooks library was built at -O0 after an optimized build failed to link
(`undefined reference to rfsB_closure`); the cause is undiagnosed (R2).
Toy: doc/recabalization-evidence/pkgtoy.

F7 [source]. Warmup imports every exposed module of every dependency
package (715 lines on this GHC for the make build's package list) so that
the whole external interface set enters GHC's EPS in one order before any
other module compiles; without it, `ghc --make -jN` object output varies
with scheduling (GHC.Core.Rules, "Note [Overall plumbing for rules]").
Today only twelve leaf modules import it, which suffices because every
other module of the monolith lies above those leaves. In a component, the
roots are different modules (Appendix A lists them), and GHC schedules a
root that does not import Warmup independently of it. The barrier exists
only where every root of the compiled unit imports the local Warmup.

F8 [reported]. The plain make build of B0 with GHC 9.14.1 (`make -j8
GHCJOBS=12 GHCRTSFLAGS='+RTS -M8G -A128m -RTS' install-src`) passed in
2:00.76 wall, 479.5 s CPU, 4.6 GB peak RSS, -O2, 252 modules; inst/lib/
Libraries held 131 files: 128 .bo, 2 .ba (foreign-function artifacts
from Randomizable's BDPI imports, version-bearing), 1 .defines. The
installed layout is the G2 oracle; the timing is not a gate here.

F9 [source]. Import graph at 9306c345 over the 250 library modules plus
app/, cross-checked against the driver's call sites (which pass runs
where, before which artifact is written). Appendix A is the result. The
stackings elab above typecheck (IExpand imports TIMonad, TypeCheck and
IConv) and schedule above elab (AAddSchedAssumps imports TypeCheck,
IExpand, ISplitIf, AConv) are genuine. Nothing in typecheck, elab or
schedule reaches a parse-exclusive module. The backends reach core, the
.ba codec and bsc-aopt.

F10 [source, approximate]. `data Flags` has 141 record fields; 77 modules
import Flags. Token-level reference counts per component: core 16, bo 3,
ba 4, parse 14, typecheck 13, elab 18, schedule 25, verilog 22, bluesim 29,
driver 63; 44 fields referenced by two or more components; 11 by none
outside Flags and FlagsDecode. These are reference counts, not action read
sets (section 4).

F11 [source]. ABinModInfo stores the full Flags (minus dump flags and
verbosity) through a hand-written `Bin Flags` instance, path-scrubbed by
remapFlagsPaths. Readers: vGenMods in app/bsc.hs regenerates Verilog from
a .ba with the stored codegen-semantic flags and 13 environment fields
overridden; it is reached from `-c` AND from the ordinary Verilog link path
for missing or stale .v files, where VFileUtils states "no options
descriptor (for now): a .v generated under different codegen flags is
reused as long as it is newer than the .ba"; BlueTcl's `module flags`
query reads them. The Bluesim path generates under the invocation's flags
and records the ones that shape the output in SimFileUtils'
codeGenOptionDescr. The `(* options *)` pragma is applied in genModule via
FlagsDecode.updateFlags after earlier package work, so a pragma option that
only affects an already-run stage is accepted and ineffective today.

## 2. Decisions taken (Ravi)

D1. bsc-core, not bsc-common (commit 252c1442).
D2. bsc-bo and bsc-ba are their own components; the files carry fingerprints.
D3. The carve is drawn at fingerprints, not at today's serialization points.
D4. Per-component Warmup is required.
D5. The evaluator component is bsc-elab.
D6. Packages, accepted with reluctance; symlink farms rather than moves
    for now; the nine-way carve, now ten with bsc-aopt, on this branch.
D7 (2026-10-01). Performance baselines and comparisons are Ravi's;
    the gates here are GHC 9.14, object determinism and Warmup efficacy.
D8 (2026-10-01, default taken by Claude, overridable). Codex's finding 14
    is accepted: the experiment does not gate P1 and P2; Flags and the
    .ba migration are later series.

## 3. The amended step 0

### 3.1 Shape

    bsc-setup      Simple  hooks library, -O0 (explicit exception to optimization: 2)
    bsc-core       Hooks   127 modules; C sources; Tcl and solver hooks; generated BuildSystem, BuildVersion, Warmup (public)
    bsc-bo         Hooks   GenBin, BinUtil
    bsc-ba         Hooks   ABin, ABinUtil, GenABin, GenForeign
    bsc-parse      Hooks   22 modules
    bsc-typecheck  Hooks   33 modules (the .bo producer: through IConv and ISimplify)
    bsc-elab       Hooks   12 modules (IExpand through AConv)
    bsc-schedule   Hooks   14 modules (the pre-.ba ASyntax stage; ASchedule and its satellites)
    bsc-aopt       Hooks   AOpt, AExpand (the ASyntax optimizer both backends run)
    bsc-verilog    Hooks   17 modules (includes ARenameIO, ADropDefs, Synthesize)
    bsc-bluesim    Hooks   17 modules
    bsc            Hooks   facade: reexported-modules = the original 250 names; the 11 executables; the 3 test suites

Every Hooks package has a three-line SetupHooks.hs importing bsc-setup.
cabal.project lists the twelve packages, sets `optimization: 2` and
`ghc-options: -j` for all of them, `jobs: $ncpus` and `semaphore: True` so
the GHC processes share one job server, and `tested-with: GHC == 9.14.1`
is stated in every package. Every package carries
`if impl(ghc < 9.14) buildable: False` and bsc-setup's configure hook
fails with a readable message on any other compiler, so a wrong GHC is a
clear error, not a mysterious one.

Why packages: F1 forbids sublibraries in a Hooks package and F7 requires a
hook in every component. Codex's alternative, checked-in Warmups for the
pinned toolchain with Simple sublibraries, is noted as viable; Ravi chose
packages (D6).

### 3.2 Why bsc-aopt exists

The Verilog driver path calls asCheck, aRenameIO, aDropDefs, aOpt and
aSynthesize (app/bsc.hs 1197-1235). aRenameIO, aDropDefs and aSynthesize
have no other callers, so they are Verilog passes. AOpt is different:
`aOpt`, its 130-line Verilog driver pass, is Verilog-only, but the Bluesim
side runs the optimizer too, in a lighter configuration: LambdaCalcUtil's
aOptAPackageLite is expandAPackage, aOptPackage1 and aOptFinalPass, and
SimPackageOpt's aExpandDynSel and aInsertCaseDef sit on the same
expression machinery. The three Bluesim-used exports reach about 85 of the
module's roughly 90 top-level definitions. So the module is shared; only
the pass is not. Putting it in core would move every fingerprint on an
optimizer edit; putting it in bsc-verilog would make Bluesim depend on the
Verilog backend. A later source split can move `aOpt` itself into
bsc-verilog and leave the machinery in bsc-aopt. AExpand is imported only by
AOpt and Synthesize and goes with AOpt.

### 3.3 Source layout

Each package is a directory holding its .cabal, SetupHooks.hs and a
symlink farm of its modules: `components/<name>/<Module/Path>.hs ->
../../src/comp/...` (and the Libs, GHC/posix, Parsec and vendor HaskellIfc
roots), generated from one manifest with a verify mode: every library
module in exactly one component, every link resolves, no unaccounted
source under the roots, every component root imports Warmup (3.4).
`hs-source-dirs: .`; no path leaves the package. Canonical files stay
where the make build expects them. Generated modules are never in the farm
and hooks never write into src/comp; make generates its own Warmup.hs and
BuildVersion.hs as today. Codex's correction stands: cabal's sdist
dereferences in-package symlinks, so the farm is not a Hackage blocker;
fresh-checkout bootstrap is "run the generator", checked in CI by
regenerating and diffing.

### 3.4 The Warmup contract

1. Every Hooks package has a hidden `other-modules: Warmup`, generated by
   its hook into the autogen directory. bsc-core's Warmup is additionally
   exposed, as the baseline exposes it, and the facade re-exports that one.
2. Content: `import M ()` for every exposed module of every DIRECT
   dependency package, external and in-place, sorted by module name, with
   the module name `Warmup` excluded (so a child never imports the public
   compatibility module into itself). The header records the resolved unit
   ids of the dependencies.
3. Every component root, defined as a module that imports nothing from its
   own component, carries `import Warmup ()`. Inside a package this
   resolves to the local hidden module (home modules win, F3); inside the
   monolithic make build it resolves to make's Warmup, so the same source
   serves both builds. Generated modules (BuildSystem, BuildVersion) are
   exempt: they import nothing external. 66 root imports are needed today;
   Appendix A lists them per component, and the generator recomputes roots
   from the graph and fails verification when one lacks the import.
4. The facade package's executables are roots of their own package; each
   app/*.hs gains the same import, and the facade's hook generates its
   Warmup like any other package.

### 3.5 The three source moves

Unchanged from Revision 1 and still the only non-Warmup source edits:
AState imports its ASchedule re-exports from AScheduleInfo and AUses
directly; isLocalAId moves from AConv to ASyntaxUtil; makeGenFuncId moves
from GenFuncWrap to GenWrapUtils.

### 3.6 Package invalidation versus action identity

The table below says which PACKAGE fingerprints an edit moves. It is not
a statement about producer actions: the .bo action also executes code in
bsc-parse and the driver, the Verilog action executes bsc-aopt and
bsc-verilog under driver orchestration, and so on. Producer action closures
are audited in P3, after the orchestration move; until then cache keys are
whole-execution (plan P2) and this experiment claims no selective reuse.

    edit in         moves F of (packages)
    bsc-schedule    schedule
    bsc-elab        elab, schedule
    bsc-typecheck   typecheck, elab, schedule
    bsc-parse       parse
    bsc-aopt        aopt, verilog, bluesim
    bsc-verilog     verilog
    bsc-bluesim     bluesim
    bsc-bo          bo
    bsc-ba          ba, parse, bluesim
    bsc-core        everything

Codex is right that core's radius is conservative rather than unavoidable:
RSchedule and AUses mix shared types with algorithms, and splitting types
from algorithms would shrink core. That is a later refinement, measured
against real edit traffic.

### 3.7 Gates

G1  cabal build all with GHC 9.14.1: twelve packages, eleven executables
    including bscdeps. Cross-DAG imports fail as "Could not find module".
G1' Surface: the facade's reexported-modules are exactly the original 250
    names; every package's module list matches the manifest; the generator's
    verify mode passes on the checked-in tree.
G1'' Determinism (Ravi's gate). From a clean dist-newstyle: build A with
    the project's parallel settings; build B the same; build C with
    `jobs: 1`, no semaphore and `-j1`. Hash every .o, .hi, .dyn_o, .dyn_hi
    and every executable; A, B and C must be identical. Then an incremental
    run: append a marker comment to one leaf module per component, rebuild,
    revert and rebuild, and compare with A again. Failure here is a design
    stop, not a mechanical fix; the first suspects are a root without the
    Warmup import (the verifier should have caught it), interface content
    of in-place dependencies not covered by the direct-dependency rule, or
    the Main modules of the facade.
G2  make -C src/Libraries build install with the cabal-built bsc bound
    explicitly (BSC=, and tconcheck/bo2bloogle from the make-installed
    inst/bin, their provenance recorded); installed inventory identical to
    the oracle including the two foreign .ba files; .bo and .ba content
    compared, BuildVersion-derived bytes accounted for separately.
G3  bscdeps in src/Libraries/Base3-Contexts versus the file probes of
    bsc -u (strace), as a smoke test of the discovery interface.
G4  Performance: not a gate here (D7). The checked-in bench harness needs
    repair before anyone relies on it: bind BSC to the cabal-built binary,
    start the compiler and bsc-only stages from equivalent states, fail on
    any failed stage.
G5  cabal test smoke, cabal test utils; the whole DejaGNU suite before
    adoption.

### 3.8 Execution

  S0.1  manifest + generator (cabal files, cabal.project, farm, verify incl. roots)
  S0.2  bsc-setup from SetupHooks.hs: per-package Warmup per 3.4; Tcl, solver,
        BuildSystem, BuildVersion for bsc-core only; GHC version check; -O0
  S0.3  the three source moves (3.5) and the 66 root imports (3.4)
  S0.4  G1 loop: one commit per mechanical fix, compiler error quoted; a
        design-level failure stops and reports
  S0.5  G1', G1'', G2, G3, G5
  S0.6  push; gate report

The experiment runs on branch claude/bsc-testsuite-cabal-dejagnu-cscgl9
on top of the applied series. It does not gate P1 or P2 (D8).

## 4. Flags re-architecture (later series, not on the critical path)

Problem as in Revision 1: one 141-field record in core, imported by 77
modules, serialized into the .ba by hand, with the driver hand-maintaining
the codegen-versus-environment partition.

Direction, amended by Codex's findings 3 and 8:

- A normalized invocation is the source of truth: command line, defaults,
  remapPathPrefix, and the module-declared options from `(* options *)`,
  with their precedence, repetition and application point specified.
- Effective options are defined PER ACTION over the action's closure, not
  per component: the .bo action's options are whatever parse, typecheck,
  IConv, LiftDicts, ISimplify and the driver orchestration read; the .ba
  action's include elab and schedule; the Verilog action's include the
  driver's Verilog path, bsc-aopt and bsc-verilog. Component views are
  projections of the invocation; a field needed by two components is a
  field of both views, filled from one value.
- The read sets come from the compiler, not from token counts: exact
  per-module field usage (for example from -ddump-minimal-imports plus
  record-field selectors), then closure over the call graph of each
  action. F10 is the sizing estimate, not the table.
- Pragma timing is preserved: the invocation-effective and module-effective
  views keep today's application point; options that cannot affect an
  already-run stage stay accepted-and-ineffective until a separate change
  warns on them. updateFlags does not run adjustFinalFlags today, and that
  stays as it is in this series.
- Codecs serialize neutral versioned metadata, never stage-owned record
  types, so bsc-ba keeps depending on core only.

Payoff unchanged: precise action keys, flag additions that move their owner
and the driver rather than core, and the hand-written `Bin Flags` gone.

## 5. The .ba envelope (later series; Addendum 1 amended)

- The payload (APackage, AScheduleInfo, pragmas including PPoptions, type,
  method dump, path info) stores no effective invocation configuration.
- The envelope carries provenance (producer fingerprint, effective .ba
  action options as neutral metadata) and the module-declared options.
  Module-declared options are not inert provenance: they enter the
  identity of every downstream action, since they may set backend options.
- Backend flags come from the driver, combined with the module-declared
  options, and key the backend action. One effective-backend descriptor
  serves direct compile, `-c`, and link-time regeneration, and the Verilog
  link path's reuse check (today "newer than the .ba") gains that
  descriptor, mirroring SimFileUtils.
- Both the module and the scheduling-error variants, and the foreign-
  function variant, carry the envelope. Exact-version rejection stays
  until the replacement is implemented and tested (plan P4).
- bluetcl's `module flags` reports the stored metadata; that and old-.ba
  compatibility are an explicit format migration, not a refactor.
- External consumers of stored flags (MatX flows, bluehs) are not audited.

## 6. Identity, fingerprints, packages

- A cabal unit id is a stable name, not F. Each package's hook has that
  package's sources, options and dependency set and can emit a
  Fingerprint module; the facade, now Hooks, can collect them. Not
  implemented in step 0: F needs the actual generated and CPP inputs, the
  recipe, the external implementation dependencies, the generator identity,
  and BuildVersion excluded, and a generated module must not hash itself.
  That is P3 work, after this experiment.
- Expectations, not earned benefits: HLS behavior with twelve packages,
  null-build overhead, Hackage legibility, collapse back to sublibraries if
  cabal issue 9986 lands.

## 7. Risks

R1. Determinism under per-component Warmup is measured by G1'', not
assumed. Candidate causes on failure listed under G1''.
R2. bsc-setup at -O0 is a workaround for an undiagnosed link failure; the
toy preserves the reproduction.
R3. Twelve packages' configure and file-monitor overhead; measured by
Ravi's harness, not here.
R4. Symlink farms: Linux and macOS only; error messages show the farm path.
R5. The 250-name surface is G1'.
R6. The P3 orchestration move is unchanged and still needed for the
driver-only-edit acceptance row and for action closures.
R7. The 66 root imports are a real source change; the make build accepts
them unchanged because its Warmup exists.
R8. Shared GHC job server (`semaphore: True`) is new to this build; if it
misbehaves, fall back to `jobs: 1` with `-j` per package.
## Revision 2.1 (2026-10-01, after G1)

- G1 passed on the first attempt with the Revision 2 shape: cabal build all,
  twelve packages, eleven executables, 3:44 wall, 733 s CPU, no fixes
  needed. In bsc-core the generated Warmup compiled third, after only the
  two trivial generated modules, and before every real module.
- Two corrections to Revision 2's analysis came from the builders: the
  partition script had never seen bird-track imports in the eleven literate
  modules (SEMonad is imported by three CVParser modules and is now in
  bsc-parse), and Version.hs is a core root (its only import is the
  generated BuildVersion). 76 root imports were added in all: 65 library
  roots plus 11 executable roots.
- A real bug in the hook design surfaced on the first partition change:
  Warmup was generated from installedPkgs of the LocalBuildInfo, the
  snapshot cabal-install takes at configure time, and cabal-install does
  not reconfigure a dependent when a dependency's exposed-modules change
  (in-place unit ids are stable). The dependent kept importing modules its
  dependency no longer exposed. Fixed by reading the current package-db
  stack with getInstalledPackages inside the rule computation; reproduced
  and repaired on the evidence toy and on this tree without a clean.
- Ravi's decision: the minimal separation that enables caching at the .bo
  and .ba level, nothing finer. bsc-aopt holds AOpt, AExpand and ACheck so
  an optimizer edit moves only the backends. The scheduling analyses,
  schedule result types and solver bridges stay in core, because the
  scheduler needs the solver bridges (so they cannot ride with the
  optimizer without re-keying .ba production) and the .ba codec and both
  backends need the schedule types (so they cannot live in the scheduler
  without making the backends depend on the frontend stack). bsc-ba depends
  on core only. The seven Bin instances for ASyntax types moved from
  BinData to GenABin: the .bo carries no ASyntax values, so the .bo codec
  no longer imports ASyntax.
- Recorded follow-ups, not done here: separate the schedule result types
  from the AUses and RSchedule algorithms (Codex finding 10), which the
  planned two-file .ba (elaborated design and schedule as separate
  artifacts, several schedules per design) makes necessary and which would
  move the algorithms into bsc-schedule; Depend is orchestration that the
  Shake layer absorbs (P5) and stays in bsc-parse for now, which is why
  parse depends on the .ba codec (isStaleABinFile); the pre- and
  post-schedule ASyntax passes split along the same line when the artifact
  does.
- G1'' found a pre-existing parity gap, not a property of the carve: two
  clean parallel builds differed in 297 object files and every executable
  while every .hi was identical. src/comp/Makefile has passed
  -fobject-determinism since GHC 9.14 added it (its comment: nondeterministic
  uniques also perturb inlining and strictness, so builds differ in the
  performance of the bsc they produce); the B0 bsc.cabal never did, so the
  cabal build at B0 was never object-deterministic. The generated project
  file now passes the flag to every component (80d01b45). With it, G1''
  passes: parallel A = parallel B = serial C = parallel A2 = the build after
  a marker edit and revert, 1152 files each. Warmup is therefore necessary
  for the interface-load order and -fobject-determinism for the object
  code; the gate measures the two together.
- Gates after G1 (G2, G3, G5, G1'') are reported in the gate report, not
  here.

## Revision 2.2 (2026-10-01, the solver carve)

- Ravi's decision: carve the solvers out of bsc-core, because the solvers
  are going to be extended or replaced. Three packages. bsc-stp (STP,
  STPFFI) and bsc-yices (Yices, YicesFFI) are the raw FFI bindings from the
  vendored HaskellIfc directories and depend on no bsc package. bsc-sat
  (SAT, AExpr2STP, AExpr2Yices, AExpr2Util, Pred2STP, Pred2Yices, SATPred)
  is every translation of a compiler IR to a solver plus the SAT facade, on
  core and the two bindings. typecheck (SATPred, which TCMisc uses for
  proviso solving), aopt (AOpt) and schedule (AProofs, ADumpSchedule,
  DisjointTest) depend on bsc-sat; no backend does. Core goes from 126 to
  118 modules, typecheck from 32 to 29; the facade surface is the same 250.
- Why the bindings are leaves: their only import from the compiler was
  ErrorUtil.internalError, 17 call sites. Each binding now defines a local
  internalError as errorWithoutStackTrace with an "Internal error in the
  STP/Yices binding" prefix. The behaviour delta is confined to binding
  misuse, a compiler bug by construction: ErrorUtil.internalError printed
  the "Internal Bluespec Compiler Error" banner with the version and exited
  1 from inside the thunk; the binding now throws ErrorCall. Both front
  ends treat the two alike at the top level (Exceptions.bsCatch in bsc and
  showrules prints the message and exits 1; HTcl.htclErrorCatcher turns
  either into a Tcl error) and no handler between the solver layer and the
  top catches one and not the other (SAT.hs catches SomeException around
  the Yices version probe only), so an internal error inside a binding
  still ends the run with exit 1, with a shorter message.
- Hooks: the vendored solver build and link configuration moved from
  bsc-core's main library to the main library of each binding package,
  keyed by package name through a table (data Solver in BscSetupHooks.hs).
  Adding a solver is a row in that table and a component in the manifest;
  replacing one is removing them. Observed on the first build: both makes
  ran concurrently under cabal -j into the shared solver prefix without
  conflict, and the rpath the leaf packages record reached the facade
  executables three packages up (RUNPATH lists both solver directories,
  ldd resolves libstp.so.1 and libyices.so.2.6), so the package-database
  propagation of F5 holds at any depth.
- What still leaks: the solver choice is the SATFlag type in Flags
  (SAT_Yices, SAT_STP) with satBackend and useProvisoSAT. Adding a solver
  adds a constructor there, a core edit, until the Flags re-architecture
  (section 4) gives bsc-sat its own curated flag type. This is the first
  concrete instance of the per-component flags argument.
- Fingerprint consequence: DisjointTest, AProofs and AOpt results depend on
  solver behaviour, so F(bsc-schedule) and F(bsc-aopt) now include
  F(bsc-sat), which includes F(bsc-stp) and F(bsc-yices), as distinct
  inputs instead of the solver being folded into F(bsc-core). A core edit
  no longer re-runs the vendored solver build at configure time, and a
  solver bump rebuilds the binding, bsc-sat and their dependents, not core.
- Gates on the carve (1d53ca52): G1 (incremental from the Revision 2.1
  tree) 2:18 wall / 388 s CPU, 99 modules recompiled, no fixes; verify
  --strict: 13 components, 2176 import edges inside the DAG, 79 component
  roots all importing Warmup; the make build of the same tree compiles the
  edited bindings (incremental make install-src, 0:58); G2 inventory
  identical to the B0 oracle (131 files), content against the oracle as
  before (embedded paths and version strings), and against this tree's own
  make-built bsc at the same source state 131 of 131 libraries
  byte-identical, the first time the cabal-built and make-built compilers
  were compared at one source state and agreed on every .bo and .ba; G3
  unchanged (78 paths named, the same 34 unlisted probes); G5 smoke PASS
  (1:36) and utils PASS (0:33); G1'' PASS: parallel A (240 s) = parallel B
  (250 s) = serial C (549 s) = parallel A2 = the build after a marker edit
  per component and revert (26 s each), 1176 files hashed each time, 23:19
  for the five builds.
- Appendix D inventories what is left in bsc-core by cluster, with each
  cluster's consumers and what carving it out would remove from core, as
  the input to the next carve decisions. The two with no compiler
  consumers are the waveform tools (D15, which carry the libfst C sources
  and zlib) and the command-line decoder (FlagsDecode in D5); the Tcl
  layer (D14) carries the Tcl link hook; the IR layers (D7+D8, D9, D10)
  are the large split and are strictly layered.
- How the carve-up proceeds from here: each carve is one short series that
  builds at every commit. A source series first, only if the move needs
  one (here, the bindings' error reporter; earlier, the AState re-exports
  and isLocalAId); then the manifest change with its generated files and
  any hook change in one commit; then `gen.py appendix` into this document
  and the gate record. gen.py gained `appendix` for that. The next carves
  on the list: the schedule result types out of the AUses and RSchedule
  algorithms (needed for the two-file .ba), Depend out of parse into the
  orchestration layer, and the per-component flag types of section 4. Each
  is a cache boundary for the Shake layer: a package's fingerprint is its
  sources plus its dependencies' fingerprints, and an action's identity
  names the fingerprints of the packages that implement it, so the finer
  the carve, the smaller the rebuild and the re-run.

## Revision 2.3 (2026-10-01, the Tcl and waveform carves)

- Ravi's decisions: carve out the Tcl binding and the waveform tools;
  both are compiler-independent and expected to leave the tree, so they
  are named for that future, without the bsc- prefix: `htcl` and
  `waveforms`. Everything that depends on the compiler keeps the prefix.
  The bluetcl glue is its own component, `bsc-bluetcl`, because bluetcl is
  already a product of its own; see the follow-up below for making it a
  package with its executable.
- `htcl` (1): HTcl with its C shim (src/vendor/htcl/haskell.c), depending
  on nothing in bsc. The Tcl include and link hook (platform.sh: tclinc,
  tcllibs, tclversion and the TCL85/TCL9 macros) moved from bsc-core's
  main library to htcl's, keyed by package name; HTcl.hs is the only
  module that needs the macros and haskell.c the only C that needs the
  headers. The Tcl libraries reach the bluetcl executable through the
  package database as the solver libraries do.
- `waveforms` (3): VCD, WaveCheck, FSTRead with the vendored libfst C
  sources (fastlz, fstapi, lz4), fstscopes_hier.c, the libfst include
  directories and zlib, depending on nothing in bsc. WaveCheck's only
  compiler import was Error(internalError), four sites; it now has a local
  reporter over errorWithoutStackTrace like the solver bindings. No
  compiler component imports any of the three; showrules, vcdcheck and
  fstcheck do, and fstscopes reaches libfst through the C.
- `bsc-bluetcl` (3): TclUtils (from core), TypeAnalysisTclUtil (from
  typecheck), BluesimLoader (from bluesim), imported only by the bluetcl
  executable; depends on core, htcl, typecheck and bluesim. With it,
  typecheck and bluesim no longer carry Tcl: bsc-typecheck goes from 29 to
  28 modules and bsc-bluesim from 17 to 16. TclUtils and BluesimLoader
  became component roots and gained the Warmup import.
- bsc-core goes from 118 to 113 modules and now carries no C sources, no
  include directories and no link configuration beyond the libstdc++ the
  B0 bsc.cabal gave every stanza (its reason is not recorded; left as
  found). Its hooks are the GHC guard, Warmup, and BuildSystem/BuildVersion.
- The determinism gate's marker edit now finds each component's module
  across every source root, so the vendored leaves (htcl, stp, yices) get
  an edit-and-revert round too; before, components whose modules live
  outside src/comp were silently skipped by that round.
- Gates: G1 cabal build all 3:59 wall / 791 s CPU, no errors; verify --strict clean (16 components, 247 farm links, every root imports Warmup); make build of the same tree at the same commit 1:10 incremental; G2 inventory identical to the B0 oracle (131 files) and 131 of 131 libraries byte-identical against this tree's make-built bsc at the same commit; G3 unchanged (78 names, the same 34 unlisted probes); G5 smoke PASS (1:44, 72 expected passes in the final summary) and utils PASS (0:32, 113); G1'' PASS: parallel A (291 s) = parallel B (254 s) = serial C (530 s) = parallel A2 = the build after a marker edit in each of the 16 components (now including HTcl, STP and Yices under src/vendor) and revert (27 s and 26 s), 1200 files hashed each time, 23:41 for the five builds.
- Follow-up recorded: a `bluetcl` package holding bsc-bluetcl's three
  modules and the bluetcl executable (bluetcl_Main.hs, bluetcl_shim.c,
  BlueTcl.hs) would give the product its own package and remove the only
  executable in the facade with its own C source; it needs executable
  stanzas in the generator and the test harness's bluetcl path to follow.
  Until then the glue library is bsc-bluetcl, since a library package named
  bluetcl beside an executable of that name in another package would make
  the bare target name ambiguous.
- On the two target ASTs Ravi asked about: Verilog can move to bsc-verilog
  after three mechanical moves, none of which touches the .ba format: the
  Bin instances for the Verilog AST in GenABin (VProgram down to VCaseArm)
  are dead, since no .ba record holds a Verilog value (the records are
  APackage, AScheduleInfo, PProp, VPathInfo, CQType, Flags and
  ForeignFunction); the keyword and identifier predicates vKeywords and
  vIsValidIdent, which PragmaCheck, IExpandUtils and FlagsDecode import,
  belong beside SystemVerilogKeywords in the base layer; and
  mkDPIDeclarations (ForeignFunction to VDPI) belongs in DPIWrappers.
  CCSyntax cannot move to bsc-bluesim while the Verilog backend's DPI and
  VPI wrapper generators emit C through it and ForeignFunctions (imported
  by the .ba codec, typecheck and elab) maps foreign types to C types with
  it; splitting ForeignFunctions into the descriptor and the two
  declaration generators would leave CCSyntax shared by the two backends
  only, a small package of its own or part of the backend-shared component.

## Appendix A. Partition, Revision 2.3 (B0 @ 9306c345), generated from util/recabal/manifest.json by `gen.py appendix`

| component | modules | depends on | import edges into each dependency |
| --- | --- | --- | --- |
| htcl | 1 | (none) | (none) |
| waveforms | 3 | (none) | (none) |
| bsc-core | 113 | (none) | (none) |
| bsc-stp | 2 | (none) | (none) |
| bsc-yices | 2 | (none) | (none) |
| bsc-sat | 7 | bsc-core, bsc-stp, bsc-yices | bsc-core (51), bsc-stp (3), bsc-yices (3) |
| bsc-aopt | 3 | bsc-core, bsc-sat | bsc-core (37), bsc-sat (1) |
| bsc-bo | 2 | bsc-core | bsc-core (24) |
| bsc-ba | 4 | bsc-core | bsc-core (51) |
| bsc-parse | 23 | bsc-core, bsc-ba | bsc-core (128), bsc-ba (1) |
| bsc-typecheck | 28 | bsc-core, bsc-sat | bsc-core (404), bsc-sat (1) |
| bsc-elab | 12 | bsc-core, bsc-typecheck | bsc-core (177), bsc-typecheck (7) |
| bsc-schedule | 14 | bsc-core, bsc-typecheck, bsc-elab, bsc-sat | bsc-core (157), bsc-typecheck (3), bsc-elab (4), bsc-sat (5) |
| bsc-verilog | 17 | bsc-core, bsc-aopt | bsc-core (167), bsc-aopt (2) |
| bsc-bluesim | 16 | bsc-core, bsc-ba, bsc-aopt | bsc-core (175), bsc-ba (6), bsc-aopt (2) |
| bsc-bluetcl | 3 | bsc-core, htcl, bsc-typecheck, bsc-bluesim | bsc-core (24), htcl (3), bsc-typecheck (1), bsc-bluesim (1) |

Total 250 modules; 1438 cross-component import edges, all inside the declared DAG.

Placements decided by driver call sites or by their own imports rather than by name: IConv, IConvLet, GroundCType, LiftDicts, ISimpDicts, ISimplify run before genBinFile (typecheck); ARenameIO, ADropDefs, Synthesize are called only by the Verilog path (verilog); AOpt and AExpand are the optimizer both backends run and ACheck the checker run beside it (aopt); SEMonad is imported by the literate CVParser modules (parse); ISyntaxCheck imports IExpandUtils and the typechecker (elab); BinParse and TclParseUtils are parser helpers (parse); the Tcl binding (HTcl with its C shim) and the waveform readers and checker (VCD, WaveCheck, FSTRead with the vendored libfst) import nothing from the compiler and are named without the bsc- prefix because they are expected to leave the tree (htcl, waveforms); the bluetcl glue (TclUtils, TypeAnalysisTclUtil, BluesimLoader) is imported only by the bluetcl executable (bluetcl); the raw solver bindings (STP, STPFFI; Yices, YicesFFI) import nothing from the compiler and are the two packages a solver is added to or replaced in (stp, yices); the translations of ASyntax expressions and CType predicates to the solvers and the SAT facade over them are one layer (sat), which is why typecheck, aopt and schedule depend on it and no backend does; the scheduling analyses and schedule result types (AUses, RSchedule, AScheduleInfo, ADumpScheduleInfo) stay in core because the scheduler, the .ba codec and both backends all need them and the types are not yet separable from the algorithms; Params, CFreeVars, ISyntaxXRef, TopUtils, ForeignFunctions and BinData are core by their imports.

**htcl (1):** HTcl

**waveforms (3):** FSTRead VCD WaveCheck

**bsc-core (113):** ADumpScheduleInfo APrims AScheduleInfo ASyntax ASyntaxUtil AUses Assump BDD BExpr Backend BackendNamingConventions Bag Balanced BinData BoolExp BoolOpt BuildSystem BuildVersion CCSyntax CFreeVars CSubst CSyntax CSyntaxTypes CSyntaxUtil CType CVPrint Changed Classic ConTagInfo CondTree DOT DefProp DynamicMap EquivalenceClass Error ErrorMonad ErrorUtil Eval Exceptions FStringCompat FileIOUtil FileNameUtil Fixity Flags FlagsDecode ForeignFunctions GHCPretty GenWrapUtils GlobPattern GraphMap GraphPaths GraphUtil GraphWrapper IOMutVar IOUtil IPrims IStateLoc ISyntax ISyntaxSubst ISyntaxUtil ISyntaxXRef IType Id IdPrint InstNodes IntLit IntegerUtil Intervals Lex ListMap ListUtil Literal Log2 MVarStrict PFPrint PPrint PVPrint Params ParseOp Position Pragma PreIds PreStrings Pred Pretty Prim ProofObligation RSchedule RealUtil SCC SchedInfo Scheme SignalNaming Sort SpeedyString StdPrel Subst SymTab SystemCheck SystemVerilogKeywords SystemVerilogTokens TopUtils Type TypeOps Undefined Unify Util VFileName VModInfo Verilog Version Warmup Wires

**bsc-stp (2):** STP STPFFI

**bsc-yices (2):** Yices YicesFFI

**bsc-sat (7):** AExpr2STP AExpr2Util AExpr2Yices Pred2STP Pred2Yices SAT SATPred

**bsc-aopt (3):** ACheck AExpand AOpt

**bsc-bo (2):** BinUtil GenBin

**bsc-ba (4):** ABin ABinUtil GenABin GenForeign

**bsc-parse (23):** BinParse CPPLineDirectives Depend Parse Parsec ParsecChar ParsecCombinator ParsecExpr ParsecPrim Parser.BSV Parser.BSV.CVParser Parser.BSV.CVParserAssertion Parser.BSV.CVParserCommon Parser.BSV.CVParserImperative Parser.BSV.CVParserUtil Parser.Classic Parser.Classic.CParser Parser.Classic.Warnings SEMonad SystemVerilogPreprocess SystemVerilogScanner TclParseUtils TmpNam

**bsc-typecheck (28):** ContextErrors CtxRed Deriving FixupDefs GenFuncWrap GenSign GenWrap GroundCType IConv IConvLet ISimpDicts ISimplify IfcBetterInfo InferKind KIMisc LiftDicts MakeSymTab PoisonUtils PragmaCheck PredTrie Simplify SolvedBinds TCMisc TCPat TCheck TIMonad TypeAnalysis TypeCheck

**bsc-elab (12):** AConv IDropRules IExpand IExpandUtils IInline IInlineFmt IInlineUtil ILift ISplitIf ISyntaxCheck ITransform IWireSet

**bsc-schedule (14):** AAddSchedAssumps AAddScheduleDefs ACleanup ADropUndet ADumpSchedule ANoInline APaths AProofs ARankMethCalls ARemoveAssumps ASchedule ATaskSplice DisjointTest WireAnalysis

**bsc-verilog (17):** ADropDefs ARenameIO AState AVeriQuirks AVerilog AVerilogUtil DPIWrappers InlineCReg InlineReg InlineWires Synthesize VFinalCleanup VIOProps VPIWrappers VPrims VStableRenumber VVerilogDollar

**bsc-bluesim (16):** LambdaCalc LambdaCalcUtil SAL SimBlocksToC SimCCBlock SimCOpt SimDomainInfo SimExpand SimFileUtils SimMakeCBlocks SimPackage SimPackageOpt SimPrimitiveModules StaleUtils SystemCWrapper VFileUtils

**bsc-bluetcl (3):** BluesimLoader TclUtils TypeAnalysisTclUtil

## Appendix B. Evidence pointers

- Branch: 61fbcb92 (0001), c0f9338e (0002), bc35d31c (0003), bc1b0abb
  (plan of record added), 252c1442 (bsc-core rename), 9f4ceb5b and
  96f13139 (Revision 1 and Addendum 1), this commit (Revision 2 and the
  checked-in toys under doc/recabalization-evidence).
- cabal-install tag cabal-install-v3.16.1.0: ProjectPlanning.hs,
  SrcDist.hs; Cabal PackageDescription.Check.Paths.
- Driver call sites: app/bsc.hs lines 497-586 (.bo producer passes),
  715 (genBinFile), 774-988 (elab and schedule passes), 1110/1139
  (genABinFile), 1197-1235 (Verilog path passes), 1227 (aOpt), 2258
  (link-path vGenMods); VFileUtils.hs 13-38; SimFileUtils
  codeGenOptionDescr; FlagsDecode.updateFlags (bsc.hs 764).
- KB drafts: "KB: REVIEW REQUEST — bsc recabalization + Flags
  re-architecture brief (B0)" (Revision 1, Codex's review, this revision's
  response block); "KB: bsc four-sublibrary carve gates (B0)"; "KB: bsc
  engine-first implementation plan (full text)"; "KB: bsc toolchain".

## Appendix C. Codex findings 1-15 and what Revision 2 does with them

1. Warmup barrier. ACCEPTED. The contract in 3.4 adds the root edges (66
   today), the generator verifies them, and G1'' measures the result with
   parallel, serial and incremental builds.
2. Packages not proven necessary. ACCEPTED as a choice (D6); the
   checked-in-Warmup alternative is recorded in 3.1.
3. Field counts are not action options. ACCEPTED. Section 4 defines
   effective options per action closure from compiler-derived read sets;
   the .bo producer's true closure (IConv, LiftDicts, ISimplify, ISimpDicts)
   is now also reflected in the partition.
4. Import DAG is not the execution graph. ACCEPTED in substance. Call-site
   analysis moved aRenameIO, aDropDefs, Synthesize to bsc-verilog and
   created bsc-aopt; 3.6 separates package invalidation from action
   identity; no selective-reuse claim is made. The three helper moves and
   the 250-name equality stand.
5. Writeback and sdist. ACCEPTED. No hook writes into src/comp; make keeps
   its own generators; the sdist symlink claim is withdrawn.
6. Warmup export contract. ACCEPTED. Same-name hidden Warmup per package,
   public one from bsc-core only, excluded from generated import lists
   (3.4); G1' checks the surface.
7. .ba consumer audit. ACCEPTED. The Verilog link path is a second reader;
   section 5 adds the reuse descriptor and treats the change as a format
   migration; external consumers are marked unaudited.
8. Pragma timing. ACCEPTED. Section 4 preserves the application point and
   the ineffective-option behavior; adjustFinalFlags unchanged.
9. Envelope specification. ACCEPTED. Neutral versioned metadata; PPoptions
   stays in the payload; module options enter downstream identity; all
   three artifact variants covered.
10. Core radius. ACCEPTED as a refinement (3.6).
11. Facade hook and fingerprint generation. ACCEPTED. The facade is Hooks;
    fingerprint generation is deferred to P3 with the listed requirements;
    bsc-setup's -O0 is stated as an exception.
12. Library build emits .ba. ACCEPTED. G2 inventories the foreign .ba
    files; the plan's ".bo-only" statement needs the qualification.
13. Harness. ACCEPTED. Performance is not a gate here (D7); the repairs are
    listed under G4 for whoever runs it.
14. Sequencing. ACCEPTED as the default (D8), Ravi's to override.
15. Evidence ledger. ACCEPTED. Facts carry labels; toys are checked in;
    raw logs of 2026-09-30 were lost to a reboot and F8 stays [reported].

## Appendix D. What is left in bsc-core (Revision 2.3), by cluster

113 modules, 47,618 lines (generated Warmup counted at 718) after Revision 2.3 took the Tcl layer (D14) and the waveform tools (D15) out; both rows are kept below, marked carved, so the table still reads against the 2.2 discussion. Levels are
the longest import chain inside core (0 = imports nothing in core); the
import DAG inside core is strictly layered base < CSyntax and types <
ISyntax < ASyntax, with the interface and annotation types (Pragma,
SchedInfo, VModInfo, Prim, Wires, DefProp) sitting at the lowest layer
that needs them. "Consumers" are the components (and executables)
importing a cluster's modules directly; counts are modules per consumer
in Appendix A's sense.

| cluster | modules | lines | consumers outside core | what carving it out would remove from core |
| --- | --- | --- | --- | --- |
| D1 build identity | BuildSystem, BuildVersion, Version, Warmup (generated) | 779 | ba, bsim, parse, sched; bsc, fstcheck, showrules, vcdcheck | nothing; stays with whatever owns ErrorUtil |
| D2 base utilities | Bag, Balanced, Sort, SCC, ListMap, ListUtil, DynamicMap, CondTree, Log2, IntegerUtil, RealUtil, Util, IOUtil, IOMutVar, MVarStrict, SpeedyString, FStringCompat, GlobPattern, Exceptions, SystemCheck, FileNameUtil, FileIOUtil, Eval, Changed, EquivalenceClass, Intervals, ErrorUtil | 3,432 | every component | a bsc-base beneath everything; FileIOUtil and SystemCheck import Error, so Error's position decides whether they belong here |
| D3 diagnostics and printing | Pretty, GHCPretty, PPrint, PVPrint, PFPrint, Classic, Position, Error, ErrorMonad | 6,405 | every component | Error (4,740 lines, the whole message catalogue) imports Flags, Position and Classic; it is the module most edited for user-facing reasons and every component depends on it, so a message change re-keys everything until Error is split into the type and the catalogue |
| D4 identifiers and names | Id, IdPrint, Fixity, PreStrings, PreIds, Literal, IntLit | 2,537 | every component | with D2 and D3, the base layer; IdPrint imports Lex (keyword tests), the one upward edge |
| D5 Flags | Flags, Backend, FlagsDecode | 2,673 | Flags: every component; FlagsDecode: bsc, bluetcl, bscdeps, bsv2bsc only | FlagsDecode (2,257 lines, the command line) is driver code no library imports; Flags itself is the re-architecture of section 4 |
| D6 lexing tables | Lex, SystemVerilogKeywords, SystemVerilogTokens, ParseOp | 1,939 | parse (Lex 4, tokens 3, keywords 2); bsc2bsv; ParseOp: bsc only | Lex is imported by CSyntax, CVPrint and IdPrint, so it stays below the C layer; ParseOp (operator fixity resolution) is driver code |
| D7 source AST (CSyntax) | CSyntax, CSyntaxTypes, CSyntaxUtil, CFreeVars, CSubst, CVPrint, GenWrapUtils, Pragma, SchedInfo, Undefined, ConTagInfo | 5,649 | parse 7, tc 20, bo 2, elab 2, ba 1, sched 1; bsc, bsc2bsv, bluetcl | a bsc-csyntax package: the .bo payload's type (with D8) |
| D8 types and inference substrate | CType, Type, TypeOps, Pred, Subst, Unify, Scheme, Assump, SymTab, StdPrel | 4,076 | tc 22 (CType) to 3 (Unify); elab (SymTab, CType); sat (CType, Pred, Type); parse (CType, Type); ba (CType) | belongs with D7 (Pred imports CSyntax and CVPrint; IType and ISyntaxUtil import StdPrel) |
| D9 internal IR (ISyntax) | IType, ISyntax, ISyntaxUtil, ISyntaxSubst, ISyntaxXRef, IPrims, IStateLoc, InstNodes, BExpr, DefProp, Wires, Prim | 6,630 | elab 12, tc 5, bo 2, sched 2; Prim: aopt, bsim, sat, vlog too | a bsc-isyntax package above D7/D8: the elaborator's input and the .bo payload's values |
| D10 ASyntax and schedule results | ASyntax, ASyntaxUtil, APrims, AUses, RSchedule, AScheduleInfo, ADumpScheduleInfo, ProofObligation, Params, SignalNaming, BackendNamingConventions | 5,985 | sched 14, bsim 15, vlog 12, sat 4, ba 3, aopt 3, elab 1 | a bsc-asyntax package above D9: the .ba payload's type; AUses and RSchedule are the algorithms that move to bsc-schedule once the schedule types are separated (Codex finding 10) |
| D11 interface and foreign descriptors | VModInfo, ForeignFunctions, BinData | 3,011 | VModInfo: every component but bo; ForeignFunctions: ba 4, vlog 4, bsim 3, elab, tc; BinData: bo, ba | BinData (the Bin class and the instances for C and I types) is the codec substrate the two codecs share; ForeignFunctions imports both target ASTs (D12) |
| D12 target ASTs | Verilog, VFileName, CCSyntax | 2,591 | Verilog: vlog 7, ba (GenABin), elab (IExpandUtils), tc (PragmaCheck), FlagsDecode, ForeignFunctions; CCSyntax: bsim 4, vlog 2 (DPIWrappers, VPIWrappers), ForeignFunctions | Verilog cannot go to bsc-verilog while GenABin, IExpandUtils and PragmaCheck import it; CCSyntax cannot go to bsc-bluesim while the Verilog DPI/VPI wrappers emit C through it |
| D13 graph and boolean algorithms | GraphMap, GraphPaths, GraphUtil, GraphWrapper, DOT, BDD, BoolExp, BoolOpt | 1,544 | sched (all four graph modules, DOT), elab (BoolExp, BoolOpt, GraphWrapper), aopt (BoolExp), vlog, bsim, tc (GraphWrapper) | pure algorithms; would ride with D2 |
| D14 Tcl (carved in 2.3: htcl, bsc-bluetcl) | HTcl, TclUtils | 1,535 | bluetcl; TypeAnalysisTclUtil (tc); BluesimLoader (bsim) | the Tcl link hook and src/vendor/htcl/haskell.c leave core; TypeAnalysisTclUtil and BluesimLoader are bluetcl-side code that would move with it |
| D15 waveforms (carved in 2.3: waveforms) | VCD, WaveCheck, FSTRead | 1,252 | none in the compiler; showrules, vcdcheck, fstcheck (and fstscopes through the C) | the libfst C sources (fastlz, fstapi, lz4), fstscopes_hier.c, the libfst include dirs and zlib leave core; the cheapest carve, zero compiler consumers |
| D16 driver utilities | TopUtils | 367 | ba, bsim, parse, sat, sched, vlog; bsc, bscdeps, bluetcl, fstcheck, showrules, vcdcheck | imports ASyntax, ISyntax, CVPrint and the tokens: a grab bag of driver helpers that each component would need to stop importing before it could move |
