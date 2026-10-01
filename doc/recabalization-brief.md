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
## Appendix A. Partition, Revision 2 (B0 @ 9306c345)

Three one-line source moves assumed (3.5). Placements by call site rather than module name: IConv, IConvLet, GroundCType, LiftDicts, ISimpDicts, ISimplify run before genBinFile (typecheck); ARenameIO, ADropDefs, Synthesize are called only by the Verilog path (verilog); AOpt and AExpand are the shared optimizer (aopt); ACheck and Params import only core (core); ISyntaxCheck imports IExpandUtils and the typechecker (elab); BinParse and TclParseUtils are parser helpers (parse); RSchedule, AUses, AScheduleInfo, ADumpScheduleInfo and AExpr2STP/AExpr2Yices/AExpr2Util stay in core because ABin, AScheduleInfo and SAT import them; CFreeVars and ISyntaxXRef are core because Parser.Classic.Warnings and FixupDefs import them.

| component | modules | imports from (import-edge counts) |
| --- | --- | --- |
| bsc-core | 127 | (none) |
| bsc-bo | 2 | bsc-core (24) |
| bsc-ba | 4 | bsc-core (51) |
| bsc-parse | 22 | bsc-ba (1), bsc-core (53) |
| bsc-typecheck | 33 | bsc-core (426) |
| bsc-elab | 12 | bsc-core (175), bsc-typecheck (7) |
| bsc-schedule | 14 | bsc-core (161), bsc-elab (4), bsc-typecheck (3) |
| bsc-aopt | 2 | bsc-core (26) |
| bsc-verilog | 17 | bsc-aopt (2), bsc-core (165) |
| bsc-bluesim | 17 | bsc-aopt (2), bsc-ba (6), bsc-core (178) |

Total 250 modules. Edges outside the intended DAG after the three one-line source moves: none.

**bsc-core (127):** ACheck ADumpScheduleInfo AExpr2STP AExpr2Util AExpr2Yices APrims AScheduleInfo ASyntax ASyntaxUtil AUses Assump BDD BExpr Backend BackendNamingConventions Bag Balanced BinData BoolExp BoolOpt BuildSystem BuildVersion CCSyntax CFreeVars CSubst CSyntax CSyntaxTypes CSyntaxUtil CType CVPrint Changed Classic ConTagInfo CondTree DOT DefProp DynamicMap EquivalenceClass Error ErrorMonad ErrorUtil Eval Exceptions FSTRead FStringCompat FileIOUtil FileNameUtil Fixity Flags FlagsDecode ForeignFunctions GHCPretty GenWrapUtils GlobPattern GraphMap GraphPaths GraphUtil GraphWrapper HTcl IOMutVar IOUtil IPrims IStateLoc ISyntax ISyntaxSubst ISyntaxUtil ISyntaxXRef IType Id IdPrint InstNodes IntLit IntegerUtil Intervals Lex ListMap ListUtil Literal Log2 MVarStrict PFPrint PPrint PVPrint Params ParseOp Position Pragma PreIds PreStrings Pred Pretty Prim ProofObligation RSchedule RealUtil SAT SCC STP STPFFI SchedInfo Scheme SignalNaming Sort SpeedyString StdPrel Subst SymTab SystemCheck SystemVerilogKeywords SystemVerilogTokens TclUtils TopUtils Type TypeOps Undefined Unify Util VCD VFileName VModInfo Verilog Version Warmup WaveCheck Wires Yices YicesFFI

**bsc-bo (2):** BinUtil GenBin

**bsc-ba (4):** ABin ABinUtil GenABin GenForeign

**bsc-parse (22):** BinParse CPPLineDirectives Depend Parse Parsec ParsecChar ParsecCombinator ParsecExpr ParsecPrim Parser.BSV Parser.BSV.CVParser Parser.BSV.CVParserAssertion Parser.BSV.CVParserCommon Parser.BSV.CVParserImperative Parser.BSV.CVParserUtil Parser.Classic Parser.Classic.CParser Parser.Classic.Warnings SystemVerilogPreprocess SystemVerilogScanner TclParseUtils TmpNam

**bsc-typecheck (33):** ContextErrors CtxRed Deriving FixupDefs GenFuncWrap GenSign GenWrap GroundCType IConv IConvLet ISimpDicts ISimplify IfcBetterInfo InferKind KIMisc LiftDicts MakeSymTab PoisonUtils PragmaCheck Pred2STP Pred2Yices PredTrie SATPred SEMonad Simplify SolvedBinds TCMisc TCPat TCheck TIMonad TypeAnalysis TypeAnalysisTclUtil TypeCheck

**bsc-elab (12):** AConv IDropRules IExpand IExpandUtils IInline IInlineFmt IInlineUtil ILift ISplitIf ISyntaxCheck ITransform IWireSet

**bsc-schedule (14):** AAddSchedAssumps AAddScheduleDefs ACleanup ADropUndet ADumpSchedule ANoInline APaths AProofs ARankMethCalls ARemoveAssumps ASchedule ATaskSplice DisjointTest WireAnalysis

**bsc-aopt (2):** AExpand AOpt

**bsc-verilog (17):** ADropDefs ARenameIO AState AVeriQuirks AVerilog AVerilogUtil DPIWrappers InlineCReg InlineReg InlineWires Synthesize VFinalCleanup VIOProps VPIWrappers VPrims VStableRenumber VVerilogDollar

**bsc-bluesim (17):** BluesimLoader LambdaCalc LambdaCalcUtil SAL SimBlocksToC SimCCBlock SimCOpt SimDomainInfo SimExpand SimFileUtils SimMakeCBlocks SimPackage SimPackageOpt SimPrimitiveModules StaleUtils SystemCWrapper VFileUtils

### Cross-component edges (importer -> imported), excluding edges into bsc-core

- bsc-bluesim -> bsc-aopt: LambdaCalcUtil->AOpt, SimPackageOpt->AOpt
- bsc-bluesim -> bsc-ba: SimCOpt->ABinUtil, SimExpand->ABin, SimExpand->ABinUtil, SimFileUtils->ABinUtil, SimPackage->ABinUtil, VFileUtils->ABin
- bsc-elab -> bsc-typecheck: IExpand->IConv, IExpand->IfcBetterInfo, IExpand->TIMonad, IExpand->TypeCheck, IExpandUtils->IConv, ISyntaxCheck->TCMisc, ISyntaxCheck->TIMonad
- bsc-parse -> bsc-ba: Depend->ABinUtil
- bsc-schedule -> bsc-elab: AAddSchedAssumps->AConv, AAddSchedAssumps->IExpand, AAddSchedAssumps->IExpandUtils, AAddSchedAssumps->ISplitIf
- bsc-schedule -> bsc-typecheck: AAddSchedAssumps->IConv, AAddSchedAssumps->TIMonad, AAddSchedAssumps->TypeCheck
- bsc-verilog -> bsc-aopt: Synthesize->AExpand, Synthesize->AOpt

### Warmup roots per component (modules importing nothing from their own component), excluding generated modules

- bsc-core: 9 roots, 0 already import Warmup, 9 need the one-line import: Balanced CondTree DynamicMap GlobPattern HTcl ListUtil SystemVerilogKeywords SystemVerilogTokens VCD
- bsc-bo: 1 roots, 0 already import Warmup, 1 need the one-line import: GenBin
- bsc-ba: 1 roots, 0 already import Warmup, 1 need the one-line import: ABin
- bsc-parse: 12 roots, 2 already import Warmup, 10 need the one-line import: CPPLineDirectives ParsecPrim Parser.BSV.CVParser Parser.BSV.CVParserAssertion Parser.BSV.CVParserCommon Parser.BSV.CVParserImperative Parser.BSV.CVParserUtil Parser.Classic.Warnings SystemVerilogPreprocess SystemVerilogScanner
- bsc-typecheck: 16 roots, 1 already import Warmup, 15 need the one-line import: FixupDefs GroundCType IConvLet ISimpDicts ISimplify IfcBetterInfo KIMisc PoisonUtils PragmaCheck Pred2STP Pred2Yices PredTrie Simplify SolvedBinds TIMonad
- bsc-elab: 3 roots, 0 already import Warmup, 3 need the one-line import: IDropRules IInlineUtil IWireSet
- bsc-schedule: 11 roots, 0 already import Warmup, 11 need the one-line import: AAddScheduleDefs ADropUndet ADumpSchedule ANoInline APaths AProofs ARankMethCalls ARemoveAssumps ATaskSplice DisjointTest WireAnalysis
- bsc-aopt: 1 roots, 0 already import Warmup, 1 need the one-line import: AExpand
- bsc-verilog: 11 roots, 0 already import Warmup, 11 need the one-line import: ADropDefs ARenameIO AVeriQuirks DPIWrappers InlineCReg InlineWires Synthesize VFinalCleanup VPrims VStableRenumber VVerilogDollar
- bsc-bluesim: 4 roots, 0 already import Warmup, 4 need the one-line import: LambdaCalcUtil SimDomainInfo SimPrimitiveModules StaleUtils

Root imports to add in total: 66.

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
