# Determinism gate

bsc's `Ord Id` compares string-intern ids, so `Data.Map`, `Data.Set` and
`sort` over `Id`s iterate in the order the process first saw each string,
and that order reaches generated files: a `bsc -u` batch or an unrelated
edit can reorder a `.v`, a `.ba`, a `.cxx` or a warning.  The hidden flag
`-reverse-intern-order` hands the ids out from `maxBound` down, which
reverses every such order and changes nothing else, so any output that
differs between a run with the flag and one without depends on intern
order.  The gate is that detector made routine: it runs the testsuite twice
in the same tree, normally and with `TEST_BSC_OPTIONS=-reverse-intern-order`,
archives every generated file after each group, and compares the two
archives byte for byte.  The suite's goldens catch 117 failures under the
flag; the bytes catch 7,611 differing files, because most leaks land in
files no golden reads (`.ba`, `.cxx`, `.bo`, the Verilog of
simulation-only tests).  `allowlist.txt` names those files with the root
cause each one was traced to.  The gate is green when nothing else differs,
and the list only shrinks.

## Files

| file | what |
|---|---|
| `run-gate.sh` | both passes over the suite, then `compare.py`; exits with its code |
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

A full run is two passes of about an hour each on 32 cores, and the
archives are about 3.7 GB per pass, so point `GATE_OUT` at a disk with
room.  `TEST_RELEASE` names the install to test (default `inst/`); the
two may also be given as positional arguments,
`run-gate.sh <inst-dir> <archive-root>`, and anything starting with `--`
is passed on to `compare.py`.

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

`.github/workflows/determinism-gate.yml` runs on `workflow_dispatch`
(inputs `groups`, empty for all, and `os`, default `ubuntu-24.04`) and on a
push to any `det/**` branch.  It is not run on pull requests: two passes of
the suite are too long for that.  One job builds bsc and the utils as the
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

`roots.md` lists every `root_id` with the source site it was traced to and
how many files of each kind it accounted for in the 2026-10-03 baseline.
Roots marked `(noise)` are not compiler ordering and belong to
`ignore.txt`, not the allow-list.
