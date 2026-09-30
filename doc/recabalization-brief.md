# bsc recabalization brief: packages, per-component Warmup, the fingerprint carve, and a Flags re-architecture

Revision 1, 2026-09-30. Base: MatX-inc/bsc release-devel-B0 @ 9306c345.
Author: Claude (gate session on Ravi's PVM), from measurements and toy
reproductions made today. Status: FOR ADVERSARIAL CRITIQUE before execution.

## 0. What this is and what we want from the reviewer

The four-sublibrary carve of bsc.cabal (commit 6c282ad7, "bsc.cabal: carve
the library into four sublibraries") was gated today and cannot build as
designed, for two reasons that are properties of cabal-install and GHC, not
typos. Fixing them changes the shape of the work: the compiler becomes a
family of cabal packages, each with its own generated Warmup module, on
disjoint source directories. Ravi's direction meanwhile moved the carve
from "packaging that mirrors the plan's four producers" to "packaging whose
units are the fingerprints we want to move independently", which is finer
(nine components), and raised a Flags re-architecture that makes those
fingerprints precise.

This brief lays out the verified facts (section 1), the decisions already
taken (section 2), the proposed step 0 (section 3), the Flags proposal
(section 4), how fingerprints and identity relate to cabal units (section
5), and the open risks (section 6). Section 7 is the execution plan.
Appendix A is the exact module partition.

Reviewer, please attack in particular:

1. The claim that per-component Warmup restores the make build's
   bit-deterministic output. The 6/6 validation in "KB: bsc toolchain" was
   for one GHC --make over 252 modules. Nine GHC processes, each with a
   Warmup covering its direct dependencies (in-place ones included), is a
   different experiment. Is there a reason it would not hold, or a cheaper
   arrangement that does?
2. Packages versus sublibraries: is there any cabal-install mechanism we
   missed that gives per-component builds to a Hooks package, or per-
   component generated modules to a Simple package, without writing into
   the source tree?
3. The Flags re-architecture: the partition of 141 fields into per-
   component records, the treatment of the 44 shared fields, and whether
   the .ba should store anything but the backend's curated record.
4. The DAG itself (Appendix A): the three one-line source moves and the
   placements that differ from the module names' natural reading.
5. Symlink farms versus directory moves for the disjoint source layout.

## 1. Verified facts

F1. cabal-install builds a Hooks package as one unit and rejects
sublibraries in it. Message at plan time, before any compilation:

    Internal libraries only supported with per-component builds.
    Per-component builds were disabled because build-type is Hooks

Source (cabal-install 3.16.1.0, cabal-install/src/Distribution/Client/
ProjectPlanning.hs): "Custom and Hooks are not implemented. Implementing
per-component builds with Custom would require us to create a new
'ElabSetup' type, and teach all of the code paths how to handle it." The
Hooks case adds CuzHooksBuildType, and checkPerPackageOk dies on any
sublibrary, private or public. The project setting `per-component` can only
disable the mode. cabal master (fetched today) keeps the exclusion with a
TODO pointing at haskell/cabal issue 9986, "Make Setup a separate
component", open, last touched 2024-05-08. Consequence: no sublibrary carve
of any granularity can live inside a Hooks package.

F2. The generated modules must stay with core. BuildSystem, BuildVersion
and Warmup are imported by core leaf modules: Bag, Classic, Changed,
EquivalenceClass, BDD, GraphPaths, Exceptions, Fixity, SEMonad, RealUtil,
Log2, Version, and by SimBlocksToC, STPFFI, YicesFFI, app/bsc.hs and
app/BlueTcl.hs. So the Hooks build type cannot be pushed below core into a
tiny helper package (Warmup's whole point is being a home module of the
component that imports it; see F7).

F3. Components cannot share hs-source-dirs. GHC's finder resolves an
unqualified import from the home unit's source path before it consults
packages. Reproduced with a toy (GHC 9.14.1, cabal-install 3.16.1.0): two
sublibraries both listing a shared directory each compiled a private copy of
the module they imported from a dependency; -Wmissing-home-modules only
warned; the executable failed with

    Couldn't match expected type 'pkgb-0.1:b-y:M1.T' with actual type 'T'
      'pkgb-0.1:b-y:M1.T' is defined in 'M1' in package 'pkgb-0.1:b-y'
      'T' is defined in 'M1' in package 'pkga-0.1'

That is what would happen to Id, CSyntax and every core module inside each
producer. With disjoint directories the same toy builds and runs, and a
cross-DAG import fails with "Could not find module": GHC then enforces the
component graph at build time. Consequence: the carve's "no source file
moves" premise does not hold; every component needs a disjoint source
directory (symlink farm generated from the manifest, or git mv).

F4. `hs-source-dirs: ../dir` outside a package directory is only the
`relative-path-outside` warning at build time (Cabal's
PackageDescription.Check.Paths marks it PackageBuildWarning). sdist will
not work with it; nothing else cares.

F5. A dependency library's `ld-options` reach an executable in another
package through the package database: in the toy, `-Wl,-rpath,/opt/fake`
on pkga's library appeared as RUNPATH on pkgb's executable. So the solver
rpath hook only needs to run for the package holding the FFI modules and C
sources; executables elsewhere need no hooks.

F6. A Hooks package's `setup-depends` may name a local library in the same
cabal.project, and a hook can generate a per-package Warmup from Cabal's
installed package index. Toy: packages toy-core and toy-sem (both Hooks,
`setup-depends: ..., toy-setup`), a facade toy (Simple, reexported-modules,
executable). cabal compiled each package's setup executable against the
local toy-setup, each package received its own autogen Warmup listing every
exposed module of its direct dependencies (componentPackageDeps looked up
in installedPkgs), and toy-sem's Warmup contained `import CoreM ()` from the
in-place toy-core. The facade's executable ran. Two details learned: the
generator must use direct dependencies, not the transitive closure (GHC
refuses modules of hidden packages); and the shared hooks library must be
built at -O0, since with optimization the StaticPointers-based rules left
floated closures unresolved at link time (`undefined reference to
rfsB_closure`).

F7. What Warmup is. update-warmup.sh writes `module Warmup where` followed
by `import M ()` for every exposed module of every dependency package (715
lines on this GHC for the make build's package list). Every leaf module
imports it, so it is compiled first, and the whole external interface set,
hence every rewrite rule and instance, enters GHC's EPS in one order before
parallel compilation starts. Without it, `ghc --make -jN` object output
varies with thread scheduling (GHC.Core.Rules, "Note [Overall plumbing for
rules]"). It works only as a home module of the component being compiled:
importing an already-compiled Warmup loads its interface and its orphans,
not the 715 interfaces behind it. Consequence: in a carved build every
component needs its own Warmup, generated against that component's direct
dependencies, in-place packages included, since bsc-core's own interfaces
carry instances and rules too.

F8. The plain make build of release-devel-B0 @ 9306c345 with GHC 9.14.1
(`make -j8 GHCJOBS=12 GHCRTSFLAGS='+RTS -M8G -A128m -RTS' install-src`)
passes in 2:00.76 wall, 479.5 s CPU (462.2 user + 17.4 sys), 4.6 GB peak
RSS, -O2, 252 modules. inst/lib/Libraries holds 131 files (128 .bo, 2 .ba,
1 .defines). This is the G2 layout oracle, and it means a full compiler
rebuild is a two-minute event on this machine.

F9. Import graph at 9306c345 (analyzer over the 250 library modules plus
app/). The 6c282ad7 partition has zero cross-producer violations. A finer
parse / typecheck / elab / schedule carve is admitted by the graph with
three one-line source moves and a handful of placements (Appendix A).
Genuine stackings, not helper leaks: IExpand imports TIMonad and TypeCheck,
IConv imports TCMisc and TIMonad, ISyntaxCheck imports TCMisc and TIMonad
(elab above typecheck); AAddSchedAssumps imports TypeCheck, TIMonad,
IExpand, IExpandUtils, ISplitIf, IConv and AConv (schedule above elab: it
typechecks and elaborates the assumption expressions it generates). Nothing
in typecheck, elab or schedule reaches a parse-exclusive module, so parse is
a sibling. The backends reach only core and the .ba codec.

F10. Flags today. `data Flags` in src/comp/Flags.hs has 141 record fields.
77 modules import Flags. Distinct fields referenced per proposed component
(token-level analysis, approximate): core 16, bo 3, ba 4, parse 14,
typecheck 13, elab 18, schedule 25, verilog 22, bluesim 29, driver (app/)
63. Fields used by exactly one component: driver 31, bluesim 13, schedule
13, elab 11, verilog 9, typecheck 5, parse 2, core 2. Fields used by two or
more components: 44; the widest are entry (7 components), backend (7),
stableVerilog (5), ifcPath (5). Eleven fields are referenced by no module
outside Flags and FlagsDecode.

F11. What the .ba stores from Flags and why. ABinModInfo carries
`abmi_flags :: Flags` (and ABinModSchedErrInfo `abmsei_flags`), written by
GenABin's hand-maintained `instance Bin Flags` that serializes the record
field by field (its comment: "should automatically verify no typos at
compile-time XXX"), except dump flags and verbosity, which write nothing.
Paths are scrubbed by remapFlagsPaths before writing. Readers: (a) app/
bsc.hs, when generating Verilog from a .ba, builds `cgflags = (abmi_flags
abmi) { bdir, vdir, infoDir, ifcPath, verbosity, showCodeGen,
showElabProgress, printFlags, printFlagsHidden, printFlagsRaw, timeStamps,
showVersion, updCheck = the invocation's }` with the comment "Codegen-
SEMANTIC flags come from the .ba itself: they were recorded there at the
original compile with any (* options *) pragma applied, so the regenerated
output matches that compile by construction. Only environment/output flags
follow this invocation." (b) app/BlueTcl.hs answers `module flags <mod>`
queries from `abemi_flags`. Nothing else reads them. So the stored flags
are, in the plan's vocabulary, the backend action's effective options, and
the 13-field override list is a hand-maintained partition of Flags into
"codegen-semantic" and "environment".

## 2. Decisions already taken (Ravi, 2026-09-30)

D1. The shared component is bsc-core, not bsc-common. It holds the IRs
(CSyntax, ISyntax, ASyntax, the Verilog AST, VModInfo), the codec
primitive BinData, Id/Type/Pred/SymTab, and the Error/Position/Flags
infrastructure. "common" names a position in the import graph; "core" names
a role and a criterion for what does not belong. Commit 252c1442 on the
branch renames it.

D2. The .bo and .ba serializers are their own components, bsc-bo and
bsc-ba, because the files carry fingerprints and a format change must move
everything that reads or writes them.

D3. The carve is drawn at fingerprints, not at today's serialization
points. A boundary between elab and schedule keys no product today; it is
drawn so that a scheduler edit moves only F(schedule), and it is the hook
for a post-elaboration cache later.

D4. Per-component Warmup is required (Ravi: "Warmup is the nasty one.
Though I guess we really need per component warmup in the new world").

D5. The evaluator component is bsc-elab (bsc's own vocabulary: `-elab-only`,
`-show-elab-progress`; "eval" appears only in trace flags, "expand" only in
internal names).

Not yet decided: packages (Ravi: "Packages makes me sad. Should they?");
symlink farms versus directory moves; whether to do the nine-way carve on
this branch or the four-way first.

## 3. Proposed step 0: recabalization

### 3.1 Shape

One cabal package per component, every producer package `build-type:
Hooks`, one shared hooks library, one Simple facade:

    bsc-setup      Simple  library only; the hooks logic (today's
                           SetupHooks.hs generalized); compiled -O0
    bsc-core       Hooks   125 modules; C sources (libfst, fstscopes_hier,
                           htcl haskell.c); Tcl and solver hooks; generated
                           BuildSystem, BuildVersion, Warmup
    bsc-bo         Hooks   GenBin, BinUtil
    bsc-ba         Hooks   ABin, ABinUtil, GenABin, GenForeign
    bsc-parse      Hooks   22 modules
    bsc-typecheck  Hooks   27 modules
    bsc-elab       Hooks   18 modules
    bsc-schedule   Hooks   16 modules
    bsc-verilog    Hooks   14 modules
    bsc-bluesim    Hooks   22 modules
    bsc            Simple  reexported-modules: the original 250 names;
                           the 11 executables; the 3 test suites

Every Hooks package's SetupHooks.hs is three lines importing bsc-setup.
Each such package gets a hidden `other-modules: Warmup` generated by the
hook from its direct dependencies (F6, F7). Only bsc-core runs the Tcl and
solver hooks and generates BuildSystem and BuildVersion; its `ld-options`
rpath reaches every executable (F5). cabal.project lists the packages and
centralizes ghc-options; `optimization: 2` for all local packages (parity
with GHCOPTLEVEL = -O2 in the make build).

Why packages and not sublibraries: F1 forbids sublibraries in a Hooks
package, F7 requires hooks in every component. The alternative, a Simple
package with sublibraries whose committed five-line Warmups CPP-include
import lists that one small Hooks package writes into the source tree, is
possible and not recommended: a build writing into the tree so another
package can include it.

Why Hooks at all: of the four hook jobs, BuildSystem could be a CPP
conditional and Tcl could be `pkgconfig-depends` plus reading the version
from tcl.h; BuildVersion could be a committed placeholder plus script. The
solvers (absolute rpath into the build tree, since dist-newstyle has no
install layout the make build's `$ORIGIN/../lib/SAT` could use) and Warmup
(generation against the resolved dependency set, per component) genuinely
need build-time hooks or an external pre-step, and an external pre-step
breaks the "cabal build just works" property the bench harness measures.

### 3.2 Source layout

Each package is a directory holding its .cabal, its SetupHooks.hs and its
sources, so `hs-source-dirs: .` and no path leaves the package (F4 becomes
moot). Two ways to populate the source directories:

- Symlink farm. `components/<name>/<Module/Path>.hs -> ../../src/comp/...`
  generated by a checked-in script from one manifest, with a verify mode
  (every module in exactly one component, every link resolves, no
  unaccounted source under the roots). Canonical files stay in src/comp,
  src/comp/Libs, src/comp/GHC/posix, src/Parsec and the vendor HaskellIfc
  directories; the make path is untouched; bench.sh's marker edits to
  src/comp/*.hs still work. A module changing component is one symlink.
- Directory moves. `git mv` into the component directories and extend the
  make path's -i list. A component becomes a directory, blame survives,
  and it is the only layout `cabal sdist` (and therefore a Hackage day)
  accepts. It churns 250 files while the partition is still being argued.

Recommendation: symlink farm now, moves when the partition has settled
(P3). Either way the generated modules are not in the farm: the hook
writes them to the autogen directory (and, as today, copies Warmup.hs and
BuildVersion.hs into src/comp for the make build), so a stale src/comp copy
can never shadow a package's own.

### 3.3 The manifest and the generator

One manifest (component name, build type, module list, component deps,
extra fields such as c-sources) generates: every .cabal file, cabal.project,
the farm, and the verify report. The module lists are the ones in Appendix
A, derived from the import graph; the generator refuses a manifest whose
induced component graph has an edge outside the declared DAG, so the check
that produced "zero violations" today becomes a build-time gate.

### 3.4 The three source moves and the placements

See Appendix A. Moves: AState's import list (types re-exported by ASchedule
but defined in AScheduleInfo and AUses); isLocalAId from AConv to
ASyntaxUtil; makeGenFuncId from GenFuncWrap to GenWrapUtils. Placements:
BinParse and TclParseUtils are parser helpers (bsc-parse); LiftDicts
imports IConv (bsc-elab); RSchedule, AUses, AScheduleInfo,
ADumpScheduleInfo and AExpr2STP/AExpr2Yices/AExpr2Util stay in bsc-core
because ABin, AScheduleInfo and SAT import them; CFreeVars and ISyntaxXRef
are core because Parser.Classic.Warnings and FixupDefs import them. With
these, the induced graph has no edge outside the DAG below (F9, Appendix
A):

    bsc-core <- bsc-bo, bsc-ba, bsc-verilog
    bsc-core, bsc-ba <- bsc-parse, bsc-bluesim
    bsc-core <- bsc-typecheck <- bsc-elab <- bsc-schedule
    everything <- bsc (facade)

### 3.5 What an edit moves (the point of the exercise)

    edit in            moves F of
    bsc-schedule       schedule
    bsc-elab           elab, schedule
    bsc-typecheck      typecheck, elab, schedule
    bsc-parse          parse
    bsc-verilog        verilog
    bsc-bluesim        bluesim
    bsc-bo             bo, typecheck, elab, schedule (writers) -- see 4.5
    bsc-ba             ba, parse, schedule, bluesim
    bsc-core           everything

Core is where the IRs live, so its blast radius is unavoidable; splitting
utilities out of core would not shrink it, since everything depends on the
IRs anyway. The plan's product-identity cutoff (equal regenerated content
stops downstream work) still applies within a moved fingerprint.

### 3.6 Gates for step 0

G1  cabal build all: every package, the facade, all 11 executables
    including bscdeps; GHC reports no cross-DAG import (F3 makes this a
    build failure, not a check).
G1' facade surface: reexported-modules is exactly the original 250 names.
G1'' determinism: build twice from clean; every .o/.hi and every
    executable bit-identical. (New; see risk R1.)
G2  make -C src/Libraries build install with the cabal-built bsc; installed
    layout identical to the oracle (F8), and .bo/.ba content compared.
G3  bscdeps in src/Libraries/Base3-Contexts against bsc -u (strace the
    file probes).
G4  util/rebuild-bench/bench.sh: null, clean-outputs, verilog-edit,
    library-edit, then the full set; compare with the monolithic B0 cabal
    build measured once as the baseline.
G5  cabal test smoke, cabal test utils; the full DejaGNU suite before
    adoption.

## 4. Flags re-architecture

### 4.1 Problem

Flags is one 141-field record in bsc-core imported by 77 modules (F10).
Every flag addition, including a Verilog-only one, changes bsc-core and so
moves every fingerprint. The record is also the backend action's effective
options as stored in the .ba (F11), so a typecheck-only flag addition
changes the .ba format and the hand-written Bin instance. And the driver
already maintains, by hand, the partition of Flags into codegen-semantic
and environment fields (the 13-field override in cgflags).

### 4.2 Proposal

- Each component owns a curated record of the flags it reads:
  ParseFlags, TypecheckFlags, ElabFlags, SchedFlags, VerilogFlags,
  BluesimFlags, and a small DiagFlags in core for what every component
  reads (verbosity, warning and error control, the `entry` and `backend`
  selectors; F10 found 44 shared fields, most of them shared between two
  or three neighbours rather than by all).
- The driver package owns the full `Flags`: FlagsDecode (command line,
  defaults, help text, `(* options *)` pragma application, remapPathPrefix)
  moves out of core into the bsc package, and Flags becomes a record of
  the component records plus the driver-only fields (31 of the 141 are
  driver-only today). Components never see the full record.
- Shared fields that a component needs are fields of its own record; the
  driver fills every copy from one command-line value. Duplication is the
  price of independence; the alternative, a shared record in core, makes
  every shared flag addition a core change again.
- Each component record derives a canonical serialization used for both
  .ba storage and fingerprinting, replacing the hand-written `Bin Flags`.

### 4.3 What it buys

- action = H(kind, F(producer), effective options, inputs) gets precise
  effective options: the .bo action is keyed by TypecheckFlags (and
  ParseFlags), the .ba action by ElabFlags and SchedFlags, the Verilog
  action by VerilogFlags. A Verilog flag no longer moves the .bo key.
- A flag addition changes the owning component and the driver, not core.
- The .ba stores the backend's curated records (VerilogFlags, BluesimFlags,
  the codegen part of DiagFlags) instead of the whole Flags. The 13-field
  override disappears structurally: environment fields are not in those
  records at all. bluetcl's `module flags` reports the stored records.
- The `Bin Flags` typo hazard the code comments on goes away.

### 4.4 Open questions for the reviewer

- Should any non-codegen flag remain in the .ba? Evidence says no
  consumer reads one (F11), but bluetcl's `module flags <mod> all` shows
  whatever is stored, and users may rely on seeing, say, the scheduling
  flags a module was compiled with. Storing SchedFlags too costs nothing.
- `(* options *)` applies per-module flag overrides inside genModule. With
  curated records the pragma must be applied to the driver's Flags and
  re-projected; is there a case where a pragma changes a flag read by a
  stage that already ran?
- Field-level analysis is token-based (approximate); the executable plan
  needs an exact per-module read set, which GHC's -ddump-minimal-imports
  plus record-field usage can give.

### 4.5 Sequencing and cost

After step 0 compiles (G1 green), as its own series: it touches the
signatures of most of the 77 modules and is behavior-preserving, so the
whole testsuite is the gate. It is source-level work, not packaging, and it
is what turns the carve's fingerprints from "which component changed" into
"which options changed".

## 5. Identity, fingerprints, packages, Hackage

- A cabal unit id is a stable name, not a content hash: in-place packages
  are `bsc-core-2026.1-inplace`, and even store unit ids hash the sdist and
  the configuration, not the semantics the plan wants (a body-only bug fix
  must move F). So the plan's F stays as defined: computed by the build
  from the component's actual inputs and its dependencies' F, never a git
  revision or a GHC ABI hash. With one Hooks package per component, each
  package's hook has exactly that component's sources, options and
  dependency set in hand, can compute F and emit a `Fingerprint` autogen
  module; the facade's hook collects them. This dissolves the registry
  problem in the plan's fingerprint section.
- Packages versus sublibraries as identity: both give stable unit ids and
  module provenance in GHC's messages (`bsc-core-2026.1` versus
  `bsc-2026.1:bsc-core`). Packages additionally have their own PackageId,
  build process, dist directory, `cabal repl`/haddock target and HLS
  support (HLS has handled multi-package projects well for years and
  sublibraries poorly). The identity is marginally better; the reason for
  packages is F1 and F7, not identity.
- Hackage, if that day comes: a package family (bsc, bsc-core, bsc-parse,
  ...) is the conventional and legible rendering; think ghc-lib, lsp,
  amazonka. Sublibraries render as one package page with components and
  weaker tooling. sdist requires real files inside each package directory,
  so the symlink farm becomes directory moves on that day; nothing else
  changes.
- If cabal issue 9986 lands and Hooks packages gain per-component builds,
  the packages collapse back into sublibraries by concatenating files.

## 6. Risks and open questions

R1. Determinism under per-component Warmup is asserted, not measured. Gate
G1'' measures it. If it fails, candidates are: Warmup must also cover
transitive dependencies (the hook can compute the closure and expose the
packages), or the executables' Main modules need Warmup too (today they are
compiled in the same --make as everything; in the carve each Main compiles
alone in the Simple facade, which has no hooks).
R2. The shared hooks library needs -O0 (F6). Confirm this is the
StaticPointers floating issue and not something the real hooks would also
hit at -O0.
R3. Null-build overhead: ten packages means ten file-monitor checks and,
when anything changed, per-package setup invocations. bench.sh's null
scenario measures it against the monolithic baseline.
R4. Symlink farms: fine on Linux and macOS, not on Windows (not a target);
error messages show the farm path (editors follow the link).
R5. The facade's 250-name guarantee is a gate; bluehs and the testsuite
depend on it.
R6. The P3 orchestration move (genModuleVerilog / genModuleC sequencing
and the getIOPropsA call site out of app/bsc.hs) is unchanged by this
brief and still needed for the driver-only-edit acceptance row.
R7. Flags re-architecture scope: 77 modules' signatures; the analysis is
approximate until done with the compiler's help (4.4).

## 7. Execution plan

Step 0 (this branch, claude/bsc-testsuite-cabal-dejagnu-cscgl9, on top of
the applied series and the two extra commits):

  S0.1  manifest + generator (cabal files, cabal.project, farm, verify)
  S0.2  bsc-setup library from SetupHooks.hs (per-package Warmup via the
        installed package index; Tcl/solver/BuildSystem/BuildVersion for
        bsc-core only; -O0)
  S0.3  the three one-line source moves
  S0.4  G1 loop (one commit per mechanical fix, compiler error quoted)
  S0.5  G1', G1'' (determinism), G2, G3, G5, then G4 alone
  S0.6  push; gate report

Then: the Flags series (section 4), then the plan's P0 measurements on the
carved build, then P1 onward as written.

Recommendation on "critique or execute": critique first, for three
reasons. R1 is a claim the whole design leans on and only a build can
settle; the Flags partition is a proposal without an exact field
assignment; and the layout choice (3.2) is a taste decision with a Hackage
consequence. The mechanical part of step 0 (S0.1-S0.4) is uncontroversial
and can start in parallel with the review if Ravi wants the clock running.

## Appendix A. Proposed partition of the 250 library modules (B0 @ 9306c345)

| component | modules | imports from (import-edge counts) |
| --- | --- | --- |
| bsc-core | 125 | (none) |
| bsc-bo | 2 | bsc-core (24) |
| bsc-ba | 4 | bsc-core (51) |
| bsc-parse | 22 | bsc-ba (1), bsc-core (53) |
| bsc-typecheck | 27 | bsc-core (339) |
| bsc-elab | 18 | bsc-core (262), bsc-typecheck (7) |
| bsc-schedule | 16 | bsc-core (170), bsc-elab (5), bsc-typecheck (2) |
| bsc-verilog | 14 | bsc-core (143) |
| bsc-bluesim | 22 | bsc-ba (6), bsc-core (231) |

Edges outside the intended DAG after the three one-line source moves: none.

The three source moves this assumes (each a one-line change plus an import list):

1. AState imports AScheduleInfo(..), ExclusiveRulesDB, areRulesExclusive, MethodUsesMap, MethodUsers, MethodId(..), UniqueUse(..) from ASchedule; every one is defined in AScheduleInfo.hs or AUses.hs (both bsc-core) and only re-exported by ASchedule. Import them from their defining modules.
2. isLocalAId (two lines in AConv) is used by AExpand (bsc-bluesim) and ADropDefs (bsc-schedule). Move it to ASyntaxUtil (bsc-core).
3. Depend (bsc-parse) imports makeGenFuncId from GenFuncWrap (bsc-typecheck). Move makeGenFuncId to GenWrapUtils (bsc-core).

Placements that differ from the natural reading of a module's name: BinParse and TclParseUtils are parser helpers (bsc-parse); LiftDicts imports IConv (bsc-elab); RSchedule, AUses, AScheduleInfo, ADumpScheduleInfo, AExpr2STP/AExpr2Yices/AExpr2Util stay in bsc-core because ABin, AScheduleInfo and SAT (all core) import them; CFreeVars and ISyntaxXRef are core because Parser.Classic.Warnings and FixupDefs import them.

### bsc-core (125)

ADumpScheduleInfo AExpr2STP AExpr2Util AExpr2Yices APrims AScheduleInfo ASyntax ASyntaxUtil AUses Assump BDD BExpr Backend BackendNamingConventions Bag Balanced BinData BoolExp BoolOpt BuildSystem BuildVersion CCSyntax CFreeVars CSubst CSyntax CSyntaxTypes CSyntaxUtil CType CVPrint Changed Classic ConTagInfo CondTree DOT DefProp DynamicMap EquivalenceClass Error ErrorMonad ErrorUtil Eval Exceptions FSTRead FStringCompat FileIOUtil FileNameUtil Fixity Flags FlagsDecode ForeignFunctions GHCPretty GenWrapUtils GlobPattern GraphMap GraphPaths GraphUtil GraphWrapper HTcl IOMutVar IOUtil IPrims IStateLoc ISyntax ISyntaxSubst ISyntaxUtil ISyntaxXRef IType Id IdPrint InstNodes IntLit IntegerUtil Intervals Lex ListMap ListUtil Literal Log2 MVarStrict PFPrint PPrint PVPrint ParseOp Position Pragma PreIds PreStrings Pred Pretty Prim ProofObligation RSchedule RealUtil SAT SCC STP STPFFI SchedInfo Scheme SignalNaming Sort SpeedyString StdPrel Subst SymTab SystemCheck SystemVerilogKeywords SystemVerilogTokens TclUtils TopUtils Type TypeOps Undefined Unify Util VCD VFileName VModInfo Verilog Version Warmup WaveCheck Wires Yices YicesFFI

### bsc-bo (2)

BinUtil GenBin

### bsc-ba (4)

ABin ABinUtil GenABin GenForeign

### bsc-parse (22)

BinParse CPPLineDirectives Depend Parse Parsec ParsecChar ParsecCombinator ParsecExpr ParsecPrim Parser.BSV Parser.BSV.CVParser Parser.BSV.CVParserAssertion Parser.BSV.CVParserCommon Parser.BSV.CVParserImperative Parser.BSV.CVParserUtil Parser.Classic Parser.Classic.CParser Parser.Classic.Warnings SystemVerilogPreprocess SystemVerilogScanner TclParseUtils TmpNam

### bsc-typecheck (27)

ContextErrors CtxRed Deriving FixupDefs GenFuncWrap GenSign GenWrap IfcBetterInfo InferKind KIMisc MakeSymTab PoisonUtils PragmaCheck Pred2STP Pred2Yices PredTrie SATPred SEMonad Simplify SolvedBinds TCMisc TCPat TCheck TIMonad TypeAnalysis TypeAnalysisTclUtil TypeCheck

### bsc-elab (18)

AConv GroundCType IConv IConvLet IDropRules IExpand IExpandUtils IInline IInlineFmt IInlineUtil ILift ISimpDicts ISimplify ISplitIf ISyntaxCheck ITransform IWireSet LiftDicts

### bsc-schedule (16)

AAddSchedAssumps AAddScheduleDefs ACleanup ADropDefs ADropUndet ADumpSchedule ANoInline APaths AProofs ARankMethCalls ARemoveAssumps ARenameIO ASchedule ATaskSplice DisjointTest WireAnalysis

### bsc-verilog (14)

AState AVeriQuirks AVerilog AVerilogUtil DPIWrappers InlineCReg InlineReg InlineWires VFinalCleanup VIOProps VPIWrappers VPrims VStableRenumber VVerilogDollar

### bsc-bluesim (22)

ACheck AExpand AOpt BluesimLoader LambdaCalc LambdaCalcUtil Params SAL SimBlocksToC SimCCBlock SimCOpt SimDomainInfo SimExpand SimFileUtils SimMakeCBlocks SimPackage SimPackageOpt SimPrimitiveModules StaleUtils Synthesize SystemCWrapper VFileUtils

## Appendix B. Cross-component edges (importer -> imported), for review

- bsc-bluesim -> bsc-ba: SimCOpt->ABinUtil, SimExpand->ABin, SimExpand->ABinUtil, SimFileUtils->ABinUtil, SimPackage->ABinUtil, VFileUtils->ABin
- bsc-elab -> bsc-typecheck: IConv->TCMisc, IConv->TIMonad, IExpand->IfcBetterInfo, IExpand->TIMonad, IExpand->TypeCheck, ISyntaxCheck->TCMisc, ISyntaxCheck->TIMonad
- bsc-parse -> bsc-ba: Depend->ABinUtil
- bsc-schedule -> bsc-elab: AAddSchedAssumps->AConv, AAddSchedAssumps->IConv, AAddSchedAssumps->IExpand, AAddSchedAssumps->IExpandUtils, AAddSchedAssumps->ISplitIf
- bsc-schedule -> bsc-typecheck: AAddSchedAssumps->TIMonad, AAddSchedAssumps->TypeCheck

## Appendix C. Evidence pointers

- Branch state: 61fbcb92 (0001), c0f9338e (0002), bc35d31c (0003),
  bc1b0abb (doc/engine-first-plan.md added from the gate kit), 252c1442
  (bsc-common -> bsc-core).
- cabal-install source consulted: tag cabal-install-v3.16.1.0 and master,
  ProjectPlanning.hs; Cabal/src/Distribution/PackageDescription/Check/
  Paths.hs.
- Toys (session scratchpad): shadowtest (F3, F5), pkgtoy (F6).
- Oracle build log and timing (F8); Libraries listing saved for G2.
- KB drafts: "KB: bsc four-sublibrary carve gates (B0)" (gate record),
  "KB: bsc four-sublibrary carve patch (B0)" (patch payload), "KB: bsc
  engine-first implementation plan (full text)" (plan of record).
