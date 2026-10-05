# Determinism gate

bsc's `Ord Id` compares string-intern ids, so `Data.Map`, `Data.Set` and
`sort` over `Id`s iterate in the order the process first saw each string,
and that order reaches generated files: a `bsc -u` batch or an unrelated
edit can reorder a `.v`, a `.ba`, a `.cxx` or a warning.  The hidden flag
`-reverse-intern-order` hands the ids out from `maxBound` down, which
reverses every such order and changes nothing else, so any output that
differs between a run with the flag and one without depends on intern
order.  The gate is that detector made routine: it runs the testsuite twice,
normally and with `TEST_BSC_OPTIONS=-reverse-intern-order`, archives every
file each run generates, and compares the two archives byte for byte.  On this line's first baseline (2026-10-04, bsc
71e1b19b) the suite's goldens catch 186 failures under the flag; the
bytes catch 13,517 differing files, because most leaks land in files no
golden reads (`.ba`, `.cxx`, `.bo`, the Verilog of simulation-only tests).
`allowlist.txt` names those files with the root cause each one was traced
to, or `UNCLASSIFIED` for the ones this line alone shows (it lacks the
determinism fixes det/base-b0 carries).  The gate is green when nothing else differs,
and the list only shrinks.

## Files

| file | what |
|---|---|
| `run-gate.sh` | both passes over the suite, at the same time in two temporary worktrees, then `compare.py`; exits with its code |
| `archive-pass.sh` | one pass: runs the suite (`make -j checkparallel`, or one group at a time with `SERIAL=1`) and archives what it generated |
| `compare.py` | compares two archives against the allow-list and the ignore list |
| `allowlist.txt` | known-pending files: `path TAB root_id TAB note`, sorted by path, `#` comments |
| `ignore.txt` | harness noise: `fnmatch-glob TAB reason` |
| `roots.md` | the classifier's roots table: what each `root_id` is and where it lives in `src/comp` |

Paths are relative to `testsuite/` with no leading `./`
(`bsc.verilog/foo/bar.v`).

## Running it locally

Build bsc and the utils first; the gate compares the dumpbo and dumpba
outputs too, so it needs `inst/bin/dumpbo` and friends:

    make install-src
    make -C src/comp install-utils
    GATE_OUT=/scratch/gate util/determinism/run-gate.sh

`TEST_RELEASE` names the install to test (default `inst/`); the two may
also be given as positional arguments, `run-gate.sh <inst-dir>
<archive-root>`, and anything starting with `--` is passed on to
`compare.py`.  The archives are about 4 GB per pass, so point `GATE_OUT`
at a disk with room.

### How long it takes

Measured 2026-10-05 on a 32-core machine that two other testsuite runs
were sharing (the load averages are quoted), bsc ee766708, all 31
groups (886 `.exp` files), the two passes at the same time with 16 jobs
each (`JOBS=32`, the default there):

| run | load average at start / end | normal pass | reversed pass | gate |
|---|---|---|---|---|
| first in the archive root (no timing table, directory order) | 41 / 18, peak 57 | 1012 s | 1003 s | 1023 s |
| second (slowest tests first, from the first run's table) | 15 / 30, peak 34 | 502 s | 508 s | 514 s |

A pass's time is `make clean`, the test list, the suite (`make`: 937 s
and 475 s) and the archiving; the gate's time runs from the start of
`run-gate.sh` to the `compare.py` verdict, two worktree checkouts
included.  The second run's longest test directory
(`bsc.bugs/perf-creg-blowup`, 414 s of its own) is most of its pass:
with the slowest tests first, a pass is close to the floor of its
longest test.  The serial gate this replaces took 4318 s and 4368 s for
its two passes of the same suite (the 2026-10-04 baseline run, bsc
71e1b19b): two hours and twenty-five minutes against eight and a half
minutes.

Both runs gave the same test results (normal 39731 pass, 2 fail;
reversed 39541 pass, 192 fail), the same archives (40422 and 40594
files, 4.1 GB each; the normal archives of the two runs hold exactly the
same paths) and the same verdict against this allow-list: 13224
differences allowed, 125 entries stale and 15 (first run) or 17 (second)
files new.  The stale entries are the 124 `*.filtered` leftovers
explained under [Equivalence](#equivalence-with-the-serial-mode) plus
`bsc.interra/OVL/assertFrame3/assertFrame3.bsc-vcomp-out`, identical
now.  The new files are the 14 `.bo`, `.ba`, dump and transcript files
of `bsc.verilog/instports`, a test added after the baseline (its
`Coll4.bs.bsc-vcomp-out.diff-out` exists in the reversed archive only),
and one to three transcripts whose only difference is `Verilog file
created:` against `Verilog file reused:`
(`bsc.bsv_examples/MacTestBench/mkSimpleSwitch.bsc-vcomp-out` in both
runs, `bsc.lib/FloatingPoint/sysFloatTest.bsc-vcomp-out` and
`bsc.syntax/bh/bh_pragmas/sysPragmas.bsc-vcomp-out` in the second).
bsc reuses a module's `.v` when its mtime is not older than its `.ba`'s,
at one-second resolution (`StaleUtils.allFreshVs`), so when a later step
of a test rewrites the `.ba` the message depends on whether that write
crossed a second boundary: it is timing noise, the stale `assertFrame3`
entry is the same flip caught the other way by the baseline, and these
transcripts belong to a normaliser or to `ignore.txt`, not to the
allow-list.  Against the baseline's serial normal archive the parallel
normal archive differs in path set only by the 900 `*.filtered`
leftovers (serial only) and the 31 files of `bsc.verilog/instports`
(parallel only); no file is missing for a timing or load reason.

### How the passes run

`archive-pass.sh` runs the suite once with `make -j$JOBS checkparallel`
(`testsuite/suitemake.mk`: every `.exp` from its own directory, as the
main CI runs it) with `DO_INTERNAL_CHECKS=1`, `TEST_RELEASE` and the
pass's `TEST_BSC_OPTIONS`, then archives every git-ignored regular file
except objects, archives, executables, `*.log`, `*.sum` and the parallel
driver's own `all_tests.mk` and `timing.txt`.  The per-directory
`testrun.sum` files are kept under `_sum/` with their paths, and
`_sum/<group>.sum` is the concatenation of a group's directories' sums,
so a consumer of the old per-group layout (PASS/FAIL lines per group)
keeps working; the make transcript is `_log/checkparallel.log`.

A group means what `make <group>.group` ran.  That target hands runtest
the basenames of the `.exp` files under `bsc.<group>/`, and dejagnu runs
every file of that basename anywhere under the `bsc.*` directories, so a
group also runs the same-named tests of other groups: `scheduler` runs
`bsc.names/portRenaming/prefixTests/methods/methods.exp`, `verilog` runs
`bsc.bluesim/vcd/vcd.exp` and seven more, and `long_tests` is 16 tests
that all live in other groups' directories.  `archive-pass.sh` resolves
each group the same way and passes the resolved `.exp` paths as
`TESTDIRS`, so `ONLY_GROUPS` selects the same tests in both modes
(checked against the serial run's sums for all 31 groups on 2026-10-05);
the map is written to `_log/groups.map`.  Without `ONLY_GROUPS` every
`.exp` runs once.

`run-gate.sh` runs the two passes at the same time, each with half of
`JOBS`, in two temporary detached worktrees of HEAD under
`<archive-root>/trees/` (a pass writes into its testsuite tree).  bsc
records the absolute path of every source file in the `.bo` and `.ba` it
writes (and the transcripts quote it), so the two trees must be seen at
the same path or every such file would differ: each pass runs in a
private mount namespace with its worktree bind-mounted over this
checkout's path (`unshare --user --mount`), and `setpriv` then drops the
namespace's capabilities so that file modes keep their meaning for the
tests that make files unreadable (a user namespace's root would read
them).  A pre-flight probe tries exactly that; where unprivileged user
namespaces are not available the passes run one after the other in this
checkout, which `SEQUENTIAL=1` asks for explicitly.  The temporary
worktrees check out HEAD: uncommitted changes under `testsuite/` are not
in them (the script warns), while `archive-pass.sh` itself runs from a
copy of this checkout's version.

### Knobs

| variable | default | effect |
|---|---|---|
| `JOBS` | the CPU count | `make -j` for the suite; concurrent passes get half each, sequential passes all of it |
| `SEQUENTIAL=1` | off | the passes one after the other in this checkout, with all the jobs; also the automatic fallback |
| `KEEP_TREES=1` | off | keep `<archive-root>/trees/{normal,reversed}` after the run |
| `GATE_TIMING` | `<archive-root>/timing.txt` | a timing table (`_log/timing.txt` of an earlier pass; every run leaves its normal pass's table at the default) that starts the slowest test directories first; the first run in an archive root has none and starts them in directory order |
| `SERIAL=1` | off | `archive-pass.sh`'s original mode, one `make <group>.group` at a time, for comparison: the same archive plus the `.filtered` leftovers described below, three times slower in the three-group measurement below and eight times for the full suite (4318 s against 502 s per pass) |
| `ONLY_GROUPS` | all | the groups to run, see below |

### Equivalence with the serial mode

Measured 2026-10-05 on this line (bsc ee766708, 32 cores, load
average 2 to 34 from other work): `ONLY_GROUPS="scheduler verilog bugs"`,
the same worktree, the normal flag, a serial pass (`SERIAL=1`) and then a
parallel pass (`JOBS=16`), compared with `compare.py` against an empty
allow-list and the committed `ignore.txt`.

| | serial | parallel |
|---|---|---|
| wall time | 1072 s | 337 s |
| test results | 7629 pass, 2 fail | 7629 pass, 2 fail |
| files archived | 10263 | 9829 |
| files compared (not `_sum/`, `_log/`, `time.out`) | 10001 | 9567 |

9533 files are byte-identical, 30 are ignored, 4 differ and 434 are
present only in the serial archive; nothing is present only in the
parallel one.  The 4 that differ are run-time noise, not order:
`bsc.bugs/bluespec_inc/b1197/FixedPointLibrary.bo.dumpbi-out.diff-out`
and `bsc.bugs/bluespec_inc/b848/sysFIFOPush.v.filtered.diff-out` (the
`+++`/`---` header timestamps of harness diffs),
`bsc.bugs/bluespec_inc/b1490/VsortWorkaround.bsv.bsc-vcomp-out` (the
`+RTS -s` GC table) and `bsc.verilog/elab_only/elabv.bsc-out` (the
`elapsed time:` lines of a `-v` transcript); all four are on the
allow-list, the first and last as `UNCLASSIFIED`, which this measurement
shows to be noise rather than leaks.  The 434 are all `*.filtered`
files, in pairs `X.filtered` / `X.expected.filtered`:
`compare_file_filtered` (`config/unix.exp`) writes them, compares them
and then runs `file delete -force` on their relative names from Tcl's
current directory, which under the serial top-level `runtest` is the
testsuite root (the external commands run in the test directory through
a `cd` wrapper, the Tcl file operations do not), so the delete silently
misses and the serial mode leaves the pair behind; under the
per-directory `runtest` of the parallel mode the delete works.  They are
filtered copies of files the gate compares anyway (the `.v` or transcript
they were made from), so nothing is lost; the 124 `*.filtered` entries
of the allow-list, which were taken from a serial baseline, are absent
under the parallel gate and are reported stale until they are removed.

The allow-list is a baseline for one bsc build: its header names the
commit.  Running the gate on a bsc that lacks fixes the baseline had, or
has fixes the baseline lacked, fails with new or stale entries that are
not leaks of the change under test; `run-gate.sh` prints the build it is
testing (`bsc -v`) for that reason.

To run some groups only (group names are the `testsuite/bsc.*` directories
without the prefix, plus `long_tests`):

    ONLY_GROUPS="scheduler names" util/determinism/run-gate.sh

With `ONLY_GROUPS` set, `run-gate.sh` passes `--scope-groups` to
`compare.py` so that allow-list entries for groups that did not run are not
reported stale (see [Scope](#scope)).

To compare two archives you already have:

    util/determinism/compare.py \
        --allowlist util/determinism/allowlist.txt \
        --ignore util/determinism/ignore.txt \
        --out /scratch/cmp \
        /scratch/gate/normal /scratch/gate/reversed

It prints a summary by extension and by `root_id` and writes `cmp.differ`
(every differing file), `cmp.new` (differing and not allowed), `cmp.stale`
(allowed but identical now, or absent from both archives), `cmp.ignored`
(matched an ignore pattern), `cmp.onesided` (present in one archive only)
and, with `--scope-groups`, `cmp.outofscope` (entries not checked for
staleness).

## In CI

The workflow is not on this branch: det/base-b0's
`.github/workflows/determinism-gate.yml` has the upstream line's Build job,
which does not build this line, and it must get this line's build job
before it is brought over.  As written there, it runs on `workflow_dispatch`
(inputs `groups`, empty for all, and `os`, default `ubuntu-24.04`) and on a
push to any `det/**` branch.  It is not run on pull requests: the serial
gate's two passes, 72 minutes each, were too long for that (see [How long
it takes](#how-long-it-takes) for the parallel gate).  One job builds bsc and the utils as the
main CI does; then five gate jobs each run `run-gate.sh` on one bundle of
groups, sized to roughly equal test counts (6224 to 6268 tests each; the
group sizes are from 2026-10-03, and `interra` alone is 5742):

| bundle | groups |
|---|---|
| 1 | interra if compile binary doc |
| 2 | verilog bugs names driver preprocessor |
| 3 | lib bsv_examples scheduler arrays options bsc_examples |
| 4 | long_tests evaluator bluesim misc real |
| 5 | mcd typechecker syntax codegen bluetcl assertions vcdcheck showrules synthesize |

The `groups` input narrows each bundle to the groups named and drops the
bundles left empty.  Every gate job uploads its compare outputs and, on a
failure, `new-files.tar.gz` with both copies of each file in `compare.new`.
A job fails on any non-zero exit from `run-gate.sh`.

### Scope

The allow-list is shared, and a bundle sees only its own groups' files, so
a bundle must not report the other bundles' entries as stale.  With
`--scope-groups "g1 g2 ..."`, `compare.py` checks staleness only for the
entries in scope:

- entries under `bsc.<g>/` for a scoped group `g`: running group `g` runs
  every `.exp` under that directory;
- entries whose directory holds at least one file archived in this run:
  this covers `long_tests`, whose `.exp` files live under other groups'
  directories, and it is exact because no testsuite directory holds more
  than one `.exp` file.

Entries out of scope are counted in the summary, listed in
`compare.outofscope` and never reported stale.  A differing file that is
not allowed fails whatever the scope.  A full run (no `--scope-groups`)
checks every entry and is the authority on staleness; a scoped run also
does not warn about ignore patterns that matched nothing, since it cannot
see most of their files.  One case escapes a scoped run: an entry whose
file is absent from both archives and whose directory produced nothing
else in this run is out of scope under rule (b); the full run reports it.

## The ratchet

1. **The allow-list only shrinks.**  A new leak found by a PR is fixed in
   the PR, not added to the list.
2. **A star removes its lines and adds a directed test.**  A fix for one
   root removes every entry carrying its `root_id` and adds a test under
   `testsuite/` that fails without the fix, since the goldens did not catch
   the leak in the first place.
3. **Stale entries fail.**  An entry whose file is identical now, or is
   absent from both archives, exits 2 until it is removed; `--allow-stale`
   is for looking, not for CI.
4. **Ignore patterns are noise only, each with a reason.**  `ignore.txt`
   is for wall-clock text, process addresses, test generators seeded from
   the time, GHC RTS statistics, the flag's own echo in `-print-flags`
   output, the `intern_order` self-test and binaries.  Compiler ordering is
   never ignored: it is allowed with a `root_id`, or fixed.
5. **Regenerate, then review the diff.**  After a fix, run the gate and
   write a fresh list:

        util/determinism/compare.py ... --write-allowlist /scratch/allowlist.new \
            /scratch/gate/normal /scratch/gate/reversed
        diff util/determinism/allowlist.txt /scratch/allowlist.new

   The diff should only remove lines, and only lines of the root the fix
   addresses; anything added is a new leak, and a changed `root_id` is a
   misclassification to look at.  `--write-allowlist` writes what differs
   in the archives it is given, so regenerate from a full run; after a
   partial run, remove the lines it reported stale by hand instead.

## Exit codes

| code | meaning |
|---|---|
| 0 | green |
| 1 | a file differs and is not allowed, or is present in one archive only and not ignored |
| 2 | a stale allow-list entry (identical now, or absent from both archives); not with `--allow-stale` |
| 3 | usage or I/O error |

## Normalisers

`compare.py` applies these before the byte comparison, and nothing else
does: in `.v .sv .cxx .h .c` files it drops lines matching `^// (On|Generated by) `
and `^ \* On ` and lines containing `--creation_time`; in `.vcd` files it
drops the `$date` section; in vvp text dumps (`*.inline-reg`,
`*.no-inline-reg`) it replaces `0x[0-9a-fA-F]+` with `0xADDR`.  Everything
else is byte-exact.

## Root ids

`roots.md` lists every `root_id` with the source site it was traced to (on
the upstream line) and how many files of each kind it accounts for in this
line's 2026-10-04 baseline.
Roots marked `(noise)` are not compiler ordering and belong to
`ignore.txt`, not the allow-list.
