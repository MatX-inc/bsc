# Naming-quality PR: state at handoff (2026-11-14)

The minimal change is finished and reviewed, but not yet ready. It is on
branch `claude/naming-library-rank`: one commit, cd442cd84, "IExpand: rank
library binder names below the design's names", on `main` (99677cb33). This
session does no further naming work; stable-output takes it from here.

## What changed from the snapshot in README.md

- The predicate is library-only: `Id.isLibraryId` (a library position, or
  `IdPLibraryBinder`, the renamed `IdPNonUserBinder`). The snapshot's
  `isUserWrittenId` also demoted compiler-generated names with no position,
  and that version changed 56 netlists (4 structurally) instead of 37.
- `moveBinder` marks a binder only when it leaves a library position for a
  non-library one.
- `improveCellName`: a moved library binder yields to an inferred
  non-library name, and otherwise keeps its name and kept def.
- `nameHeapRef` renames an existing cell only when its name is a library
  name and the new binder ranks higher (`idQuality`: keep+other 4,
  keep+library 3, non-bad+other 2, non-bad+library 1, bad 0).
- An evalHeap struct cell with a library name no longer prefixes its fields'
  names.
- Tags `bsc-bo`/`bsc-ba-20261114-23`.

## Results

- All bsc suites plus log2_loop give 0 unexpected failures, with no expected
  file changes. Five suites whose `.exp` sits at the top level ran 0 tests
  under `make check`, but match main with `localcheck` (bsc.binary, doc,
  showrules, synthesize, vcdcheck).
- 37 Verilog files are names-only, mostly the `v` of ActionValue's bind
  becoming the user's `t` in `let t <- $time`. 35 Bluesim files only rename
  defs; `h264/mkDeblockFilter.cxx` also reorders its symbol table, but drops
  no def.
- Perf fixtures +0.01% to +0.19% allocation, with identical netlists.
- A 95-package production MatX build: +0.12% allocation, +0.1% MUT, all
  netlists identical, and the AOpt time of the module that blew up when names
  were dropped is unchanged (2.2 s).
- The witness test `bsc.names/library_names` has 18 checks; the
  user-beats-library checks fail on main.

## Open review items (both reviews: not ready)

1. **Witness coverage.** No test exercises:
   - `nameHeapRef`, a later design binder renaming a library-named cell,
     which accounts for most renamed files;
   - the `improveCellName` arm where an inferred name beats a moved library
     binder;
   - the struct-prefix rule.

   Deleting any of these passes every check. Add discriminating cases (for
   `nameHeapRef`, a BSV `let t <- $time` style case, Verilog only).
2. **`improveCellName`, moved-library arm.** `Just n | not (isLibraryId n)`
   lets a bad inferred name beat a library binder, which contradicts the rank
   (`idQuality` puts bad below library). Fix: `not (isLibraryId n || isBadId n)`,
   or compare `idQuality`.
3. **CSE scope.** AConv's `pickId` restores names on CSE'd defs by taking the
   first non-bad name, without `idQuality`, so a library name can still win
   there. Either scope the claim to IExpand/ITransform (the commit message
   now does) or use `idQuality` in AConv, which is outside the minimum.
4. **The `nameHeapRef` comment** says "a binder", but struct field names
   (evalHeap move-tuple) and set-sel-pos's preserved name also reach it.
   Field naming then depends on order: a struct first named by a library
   binder keeps bare field names after a later rename.
5. **PR text** (`pr.md` in the source worktree, not on this branch). It needs
   rewording:
   - "the check never fires" should be "misses binders inlined into the
     design";
   - drop "tier" (AGENTS section 6);
   - drop the LevelFifo reference (an earlier variant's result);
   - name or describe the perf fixtures;
   - the 56-vs-37 comparison is with the whole earlier snapshot, not the
     predicate alone;
   - shorten to one to three paragraphs;
   - the commit message's earlier "drops a def" claim is fixed; the PR text
     needs the same fix.
6. **Nits.**
   - Anchor the negative regexp: `{\mx__h\d+}`.
   - A user binder on a UInt/Int view never names the Bit cell inside it.
     This is not a regression; it is a note for the hierarchy work.
7. **The install** that produced the evidence reports the snapshot's build id
   (39acf1f2a), because it was built before the commit. The binary matches the
   committed sources, apart from the tag strings, which changed afterwards.

## Tags

`readBinFile` (GenBin) and its GenABin twin compare only the first
`length header` bytes, so a tag that is a prefix of another accepts that
tag's files and then fails decoding them instead of reporting S0005. Tags used
by in-flight PRs must therefore not be prefixes of one another:

| PR | Tag |
|---|---|
| #354 (ActionValue loops) | `-1` |
| foreign-call fix, main | `-21` |
| foreign-call fix, #272 series | `-22` |
| this PR | `-23` |
| library point reductions | `-24` |

The prefix check itself is a latent bug on main and could be a small separate
fix.
