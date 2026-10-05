# Determinism gate

bsc's `Ord Id` compares string-intern ids, so `Data.Map`, `Data.Set` and
`sort` over `Id`s iterate in the order the process first saw each string,
and that order reaches generated files: a `bsc -u` batch or an unrelated
edit can reorder a `.v`, a `.ba`, a `.cxx` or a warning.  The hidden flag
`-reverse-intern-order` hands the ids out from `maxBound` down, which
reverses every such order and changes nothing else, so any output that
differs between a run with the flag and one without depends on intern
order.  The gate is that detector made routine: it runs the testsuite twice,
normally and with `-reverse-intern-order` in `TEST_BSC_OPTIONS`, archives
every file each run generates, and compares the two archives byte for
byte.  On this line's baseline (2026-10-05, bsc 3be09aa3) the suite's
goldens catch 172 failures under the flag; the bytes catch
13,403 differing files, because most leaks land in files no golden
reads (`.ba`, `.cxx`, `.bo`, the Verilog of simulation-only tests).
`allowlist.txt` names those files with the root cause each one was traced
to, or `UNCLASSIFIED` for the ones this line alone shows (it lacks the
determinism fixes det/base-b0 carries).  The gate is green when nothing
else differs, and the list only shrinks.

## Files

| file | what |
|---|---|
| `run-gate.sh` | both passes over the suite, at the same time in two temporary worktrees, then `compare.py`; exits with its code |
| `archive-pass.sh` | one pass: runs the suite (`make -j checkparallel`, or one group at a time with `SERIAL=1`) and archives what it generated |
| `compare.py` | compares two archives against the allow-list and the ignore list |
| `check-normalisers.py` | self-test of `compare.py`'s normalisers on captured samples; run it after changing one |
| `allowlist.txt` | known-pending files: `path TAB root_id TAB note`, sorted by path, `#` comments |
| `ignore.txt` | harness noise: `fnmatch-glob TAB reason` |
| `timing.txt` | a timing table from a full run on a 32-core machine: the slowest test directories start first (see [The timing table](#the-timing-table)) |
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
at a disk with room; the default, `build/determinism/`, is git-ignored.
The install and the archive root may be anywhere except the checkout
itself, anything under its `testsuite/`, or under the archive root's
`trees/`, which the gate removes.  The checkout may be a main repository
(`.git` a directory, as `actions/checkout` makes it) or a linked
worktree; the gate needs `git worktree`, `setsid` (util-linux) and
`python3`, nothing else.  The passes run in temporary worktrees of HEAD,
so uncommitted changes under `testsuite/` are not tested (the script
warns); `archive-pass.sh` and `compare.py` run from the checkout, so
uncommitted edits to them are.

### How long it takes

Measured 2026-10-05 on a 32-core machine that other testsuite runs and
two GHC builds were sharing (the load averages are quoted), all 31
groups (886 `.exp` files), the two passes at the same time with 16 jobs
each (`JOBS=32`, the default there):

| run | bsc | load average at start / end | normal pass | reversed pass | gate |
|---|---|---|---|---|---|
| first on this tree's bsc, seeded with an earlier table, other suites running | 3be09aa3 | 16 / 32, peak 47 | 544 s | 546 s | 550 s |
| second, the committed table, against the regenerated list: GREEN | 3be09aa3 | 28 / 32, peak 48 | 565 s | 567 s | 571 s |
| third, in a main repository (`git clone -s`, `.git` a directory), CI's layout (`GATE_OUT` under the checkout, no arguments), the committed table: GREEN | 3be09aa3 | 18 / 31, peak 38 | 468 s | 471 s | 475 s |
| earlier mechanism (mounts), seeded, machine otherwise idle | ee766708 | 1 / 28, peak 37 | 453 s | 458 s | 462 s |

A pass's time is `make clean`, the test list, the suite and the
archiving; the gate's time runs from the start of `run-gate.sh` to the
`compare.py` verdict, two worktree checkouts included.  With the slowest
tests first a pass is close to the floor of its longest test directory
(`bsc.bugs/perf-creg-blowup`, about 400 s of its own).  The serial gate
this replaces took 4318 s and 4368 s for its two passes of the same suite
(the 2026-10-04 baseline run, bsc 71e1b19b): two hours and twenty-five
minutes against nine and a half.  A first run with no timing table
(directory order) took 1012 s per pass on the same machine.

Both full runs gave the same test results (normal 39685 pass, 61 fail;
reversed 39511 pass, 235 fail; see [How the passes
run](#how-the-passes-run) for the 55 failures `-remap-path-prefix` adds
to both passes) and the same archives (40479 and 40635 files, 39589 and
39745 of them compared).  The first run, against the 2026-10-04 list
(bsc 71e1b19b), reported 13226 differences allowed, 128 entries stale
(the 124 `*.filtered` leftovers of the serial harness, two noise entries
the normalisers now cover, two `-v` echoes moved to `ignore.txt`) and 14
new files, all under `bsc.verilog/instports`, a test added after that
baseline; the list was regenerated from it.  The second run, against the regenerated list and
scheduled by the committed table, was GREEN: 13239 differences allowed
(13403 entries: 13239 differing, 4 present in the normal archive only,
160 in the reversed one only), 0 new, 0 stale, 139 files ignored by 47
patterns; the third run, in a main-repository clone with CI's layout,
gave exactly the same verdict and counts.

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
keeps working; the make transcript is `_log/checkparallel.log`.  A test
directory that wrote no `testrun.sum` (its `runtest` did not finish)
makes the pass exit 4 with the directories in `_log/no_sum.txt`;
`run-gate.sh` then says INCOMPLETE, still runs `compare.py` for the
record, and exits 3.

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
`JOBS`, in two temporary detached worktrees of HEAD,
`<archive-root>/trees/normal` and `trees/reversed` (a pass writes into
its testsuite tree, so two passes cannot share one).  They are plain
worktrees at their own paths: nothing is mounted and no namespace is
entered.  bsc stores the absolute path of every source file in the `.bo`
and `.ba` it writes (4163 of the 4173 `.bo` and 3422 of the 3425 `.ba`
of a full archive hold it, and the `.ba.dumpba-out` dumps print it), so
two trees at different paths would differ in every such file: each pass
therefore also gets `-remap-path-prefix <its tree>=.` in
`TEST_BSC_OPTIONS` (upstream PR 1040; `suitemake.mk`'s `RUNTESTENV`
hands the whole string to bsc as `BSC_OPTIONS`), and the stored paths
read `./testsuite/...` in both archives.  This is temporary: upstream
#1135 takes the absolute paths out of the `.bo`/`.ba`, and once this
line carries it the flag is a no-op and goes.  The flag has a visible
cost: positions that bsc or bluetcl read back from a `.bo`/`.ba` print
as `./testsuite/<dir>/X.bsv` instead of `X.bsv` (the `///` marker that
made them relative is gone with the remap), and `-print-flags` and `-v`
echo the flag, so the normal pass fails 57 tests instead of 2: 12
`-print-flags` echoes in `bsc.options`, 41 bluetcl outputs under
`bsc.bluetcl`, `bsc.misc/method_conditions` and `bsc.names`, and
`CoerceHoldEager` and `AssertionWiresTest2`, whose messages point into
an imported package.  They fail the same way in both passes, so the byte
comparison does not see them; the `-print-flags` and `-v` transcripts
were already in `ignore.txt` as the flag's own echo.

A few files still quote their tree's own path and so differ between the
two trees; they are harness noise of the "where the tree is" kind and
have patterns in `ignore.txt`, each with the message and its source:

- `bsc.batching/intern_order/reversed.out`: the test dumps the intern
  table, in which the pwd-marked source path is an interned string (not
  a position, so not remapped);
- `bsc.driver/bluesim/Top.bsv.bsc-ccomp-out`: warning S0073 lists the
  duplicate `-p` directories by absolute path (`Error.hs`
  `WDuplicatePathDirs`);
- `bsc.preprocessor/include/IncludeAbsolute.bsv*`: the test m4-expands
  its own directory into the source as an absolute `` `include ``, and
  the transcript's `Compilation message` position quotes it;
- `bsc.preprocessor/misc/*.vpp-out`: `-dvpp` dumps the preprocessed
  source with `` `line(<pwd-marked path>,...) `` directives;
- `bsc.verilog/filter/RenameTest.bsv.bsc-vcomp-out`: error G0121 quotes
  the generated Verilog file by its full path (`Error.hs`
  `EVerilogFilterError`);
- the `bsc -v` echoes (`bsc.verilog/elab_only/elabv.bsc-out`,
  `bsc.options/CTypeStats.bsv.bsc-out`, `bsc.bluesim/parallel/mkTbGCD-1`)
  carry `-remap-path-prefix <tree>=.` itself; `mkTbGCD-2` also quotes the
  tree in `exec: make -f <tree>/.../compile_mkTbGCD.mk` and stays on the
  allow-list for its link order (`bluesim-package-map-order`).

Measured on the full archives: 0 of 4173 `.bo` and 3425 `.ba` hold the
tree's path; outside `_sum/`, `time.out` and the launcher scripts that
`ignore.txt` covers, the files above are the only ones that do.


Two gates must not share an archive root: `run-gate.sh` holds
`<archive-root>/lock` (a directory with its pid) and refuses to start
while another gate holds it; a lock whose pid is gone is taken over.  On
SIGINT or SIGTERM it kills both passes (each runs in its own process
group under `setsid`), removes the worktrees unless `KEEP_TREES=1`,
releases the lock and exits 128 + the signal (measured: 1.5 s after
SIGTERM, nothing left running).  `SEQUENTIAL=1` runs the two passes one
after the other in one tree (`trees/normal`) with all the jobs, remapped
the same way, which is the mode to compare against.

### Knobs

| variable | default | effect |
|---|---|---|
| `JOBS` | the CPU count | `make -j` for the suite; concurrent passes get half each, sequential passes all of it |
| `SEQUENTIAL=1` | off | the passes one after the other in one temporary worktree, with all the jobs |
| `KEEP_TREES=1` | off | keep `<archive-root>/trees/{normal,reversed}` after the run |
| `GATE_TIMING` | `<archive-root>/timing.txt`, else the committed `timing.txt` | a timing table (`_log/timing.txt` of an earlier pass) that starts the slowest test directories first; every run leaves its normal pass's table at `<archive-root>/timing.txt`, so a second run in an archive root is scheduled by the first one's times.  The table is copied to `<archive-root>/timing.txt`, which is where the passes read it |
| `SERIAL=1` | off | `archive-pass.sh`'s original mode, one `make <group>.group` at a time, for comparison: the same archive, eight times slower for the full suite (4318 s against about 500 s per pass) |
| `ONLY_GROUPS` | all | the groups to run, see below |
| `TEST_RELEASE`, `GATE_OUT` | `inst/`, `build/determinism/` | the install and the archive root when no positional arguments are given (CI sets these) |

### The timing table

`testsuite/scripts/sort-by-time.pl` starts the slowest test directories
first when `RUN_TESTCASES_IN_ORDER_OF_TIME` is set and `timing.txt` holds
their times, which cuts the tail of a parallel pass: 1012 s against 502 s
per pass was the measured difference on the same machine between a first
run in directory order and a run with a table.  The committed
`util/determinism/timing.txt` is the table of the first of those runs (bsc 3be09aa3, 2026-10-05), so that a run
in a fresh archive root (CI's every run) is scheduled from the start;
`archive-pass.sh` copies it into the tree before `make checkparallel`,
and `generate-stats` merges the run's own times into it, which the gate
keeps as `<archive-root>/timing.txt` for the next run there.  To refresh
the committed table after a full run:

    cp <archive-root>/normal/_log/timing.txt util/determinism/timing.txt

The table only orders the tests; it changes nothing in the archives
(two full runs in directory order and slowest-first produced the same
file sets and the same comparison).

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

9533 files were byte-identical, 30 ignored, 4 differed and 434 were
present only in the serial archive; nothing was present only in the
parallel one.  The 4 that differed were run-time noise that
`compare.py` now normalises: the `+++`/`---` header timestamps of two
harness diffs, a `+RTS -s` GC table and the `elapsed time:` lines of a
`-v` transcript.  One more file differs between a serial and a parallel
archive whatever the flag: `bsc.bluesim/parallel/mkTbGCD-2.bsc-ccomp-out`
captures a `make` that bsc runs for `-parallel-sim-link`, and its
`make[1]: Entering directory` lines read `make[2]` under the parallel
driver, which adds one make level; the normaliser drops the level (and
the directory, which is the tree's path).  The 434 were `*.filtered`
files, in pairs `X.filtered` / `X.expected.filtered`, that
`compare_file_filtered` (`config/unix.exp`) wrote and then failed to
delete under the top-level `runtest`: its `file delete` used names
relative to Tcl's current directory, the testsuite root, while the files
sat in the test's directory, so every serial run left them behind (900
in the 2026-10-04 baseline, 124 of which differed and were on the
allow-list).  The proc now deletes them by the directory-joined names;
measured on `ONLY_GROUPS="scheduler names"`: the serial pass archived 300 `*.filtered` files
before the fix and 0 after, with identical results (2510 pass, 5 fail),
and the serial and parallel archives then hold the same 3392 files
(nothing one-sided; 9 differ, all identical after the normalisers).

### Equivalence of one tree and two

The concurrent gate runs its two passes in two trees, the sequential one
in one, so the question is whether a file's bytes depend on where its
tree is.  Measured 2026-10-05, `ONLY_GROUPS="scheduler verilog bugs"`,
the same install (this tree's bsc 3be09aa3), the concurrent gate
(`trees/normal` and `trees/reversed`, 16 jobs each) and then
`SEQUENTIAL=1` (both passes in `trees/normal`, 32 jobs), byte-exact
comparison of the archives, no normaliser:

| | normal archives | reversed archives |
|---|---|---|
| files | 9567 each | 9666 each |
| byte-identical | 9503 | 9503 |
| differing | 64 | 163 |
| one-sided | 0 | 0 |

Both normal passes ran in `trees/normal`, so the 64 are run-to-run
noise, and all of it is what the normalisers are for: 58 are identical
after them (harness-diff header timestamps 13, vvp heap addresses 24,
b1490 RTS tables 10, created/reused and elapsed-time transcript lines
6, VCD dates 5), 4 are ignored (the `bsc.bluesim/vcd` FST dumps, the
`ResourceOneRuleMEbug` `-print-flags` echo), and 2 quote the tree's
path: `RenameTest.bsv.bsc-vcomp-out` (G0121) and `elabv.bsc-out` (the
`-v` echo), both in `ignore.txt` now.  The reversed archives, one in
`trees/normal` and one in `trees/reversed`, differ in the same files
plus 99 more harness diffs of the flag's failures, header timestamps
only.  Test results were identical (normal 7629 pass 2 fail, reversed
7525 pass 106 fail).  Times: the concurrent gate 731 s (passes 722 s and
727 s with 16 jobs each, load 31 to 70 with other suites running), the
sequential gate 592 s (298 s + 293 s with 32 jobs, load 8 to 30 after
they had ended).

The two gates reported identical `compare.new` and `compare.stale`
(81 entries each, the 80 `*.filtered` leftovers of the
old baseline and one identical noise entry; `compare.new` differed by
exactly `RenameTest.bsv.bsc-vcomp-out`, which has an `ignore.txt` entry
now, and both named the 14 `bsc.verilog/instports` files).

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
gate's two passes, 72 minutes each, were too long for that; the parallel
gate takes nine to ten minutes for the whole suite on 32 cores (see
[How long it takes](#how-long-it-takes)), and a bundle a fifth of that.
One job builds bsc and the utils as the main CI does; then five gate jobs
each run `run-gate.sh` on one bundle of groups, sized to roughly equal
test counts (6224 to 6268 tests each; the group sizes are from 2026-10-03,
and `interra` alone is 5742):

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

What the workflow needs when it is brought over, against what it says
today:

- Nothing about namespaces: the gate needs `git worktree` and `setsid`,
  both on every Ubuntu runner, and runs the same way in `actions/checkout`'s
  main-repository shape as in a linked worktree (measured on both).
- `GATE_OUT: ${{ github.workspace }}/gate-out` works, but `gate-out/` is
  not git-ignored, so the run leaves an untracked directory in the
  workspace; dropping the variable uses the default `build/determinism/`,
  which is ignored.  `TEST_RELEASE: ${{ github.workspace }}/inst` and
  `CCACHE_DIR: ${{ github.workspace }}/ccache` are fine as they are: the
  passes see the workspace as it is.
- The timing table: nothing to do, `run-gate.sh` uses the committed
  `util/determinism/timing.txt` when the archive root has none.
- Its header comment ("two serial passes ... about an hour on 32
  cores") and the 360-minute timeout describe the serial gate; a bundle
  takes minutes now.
- The bsc it builds must know `-remap-path-prefix` (upstream PR 1040,
  which this line has) until #1135 makes the flag unnecessary.

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
   the time, the flag's own echo in `-print-flags` output, the
   `intern_order` self-test, files that quote their tree's path, and
   binaries; a known noise line inside an otherwise meaningful file gets a
   normaliser in `compare.py` instead (and a case in
   `check-normalisers.py`).  Compiler ordering is never ignored: it is
   allowed with a `root_id`, or fixed.
5. **Regenerate, then review the diff.**  After a fix, run the gate and
   write a fresh list:

        util/determinism/compare.py ... --write-allowlist /scratch/allowlist.new \
            /scratch/gate/normal /scratch/gate/reversed
        diff util/determinism/allowlist.txt /scratch/allowlist.new

   The diff should only remove lines, and only lines of the root the fix
   addresses; anything added is a new leak, and a changed `root_id` is a
   misclassification to look at.  `--write-allowlist` writes what differs
   in the archives it is given, so regenerate from a full run; after a
   partial run, remove the lines it reported stale by hand instead.  The
   written file carries a short header; keep the committed header's
   Baseline paragraph, updated to the build the archives came from.

## Exit codes

| code | meaning |
|---|---|
| 0 | green |
| 1 | a file differs and is not allowed, or is present in one archive only and not ignored |
| 2 | a stale allow-list entry (identical now, or absent from both archives); not with `--allow-stale` |
| 3 | usage or I/O error, or a pass that did not complete (INCOMPLETE in the log) |
| 130, 143 | interrupted (SIGINT, SIGTERM): passes killed, worktrees removed |

## Normalisers

`compare.py` applies these before the byte comparison, and nothing else
does; `check-normalisers.py` runs each on captured samples.  In `.v .sv
.cxx .h .c` files it drops lines matching `^// (On|Generated by) ` and
`^ \* On ` and lines containing `--creation_time`; in `model_*.cxx` also
`get_creation_time`'s `/* <ctime> */` comment and the `return <n>llu;`
after it.  In `.vcd` files it drops the `$date` section.  In vvp text
dumps (`*.inline-reg`, `*.no-inline-reg`) it replaces `0x[0-9a-fA-F]+`
with `0xADDR`.  In bsc transcripts (`.bsc-out`, `.bsc-vcomp-out`,
`.bsc-ccomp-out`, `.bsc-regen-out`, ...) it reads `Verilog file reused:`
and `<engine> object reused:` as `created:` (the choice is a one-second
mtime comparison in `StaleUtils.allFreshVs` and flips between runs),
blanks the numbers of ` elapsed time: CPU ..s, real ..s` lines, drops the
GHC RTS statistics of a run with `+RTS -s` or `-S`, and reduces `make[N]:
Entering/Leaving directory '<dir>'` to the word.  In `.diff-out` files
it drops the timestamps of the `---`/`+++` header lines.  Everything
else is byte-exact.  Measured on two independent normal passes of the
full suite (2026-10-05): after these, 0 of 39383 compared files differ
run to run (13 did before, 176 in the reversed pass).

## Root ids

`roots.md` lists every `root_id` with the source site it was traced to (on
the upstream line) and how many files of each kind it accounts for in this
line's 2026-10-05 baseline.
Roots marked `(noise)` are not compiler ordering and belong to
`ignore.txt`, not the allow-list.
