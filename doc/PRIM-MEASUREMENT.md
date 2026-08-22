# Measuring the array/list primitives on MatX RTL

Companion to the `claude/prim-fixes-integration` branch: how to measure
what the elaboration primitives (primArrayMap/FoldL/FoldR/ZipWith/GenWith/
ToList/Append/Concat/Reverse, primListToArray, primListMap/FoldL/FoldR/
Append/Concat/Length/Select/ZipWith) buy on the real MatX Bluespec
codebase, by comparing two compilers over every `bs_library` target.

## Compilers

Build both from MatX-inc/bsc, each in its own worktree:

- **A** — commit `807a06767`: the elaborator/backend performance branch
  *without* the primitive set (its `Array.bsv` still uses the two interim
  construction prims).
- **B** — branch `claude/prim-fixes-integration` (tip `d0f02ea69`): the
  full primitive set plus the spine-tail heaping fix.

```
git worktree add ../bsc-A 807a06767 && cd ../bsc-A
git submodule update --init --recursive
make install-src GHCJOBS=$(nproc) GHCOPTLEVEL='-O2'
```
(similarly for B; the compiler lands in `<worktree>/inst`.)

## Legs

Uses the matx repo's own local-compiler channel
(`third_party/bluespec/DEVELOP.md`):

1. `.user.bazelrc`:
   `common --repo_env=MATX_BSC_LOCAL=<abs path to inst A>`
2. Warm once on the default production channel so all non-Bluespec
   dependencies build:
   `bazel query "kind(bs_library,//rtl/...)" | xargs bazel build --keep_going`
3. One leg per compiler — point `MATX_BSC_LOCAL` at that compiler's
   `inst` (a *value change* refetches `@bsc-local` automatically) and run
   the same target list with
   `--//third_party/bluespec:compiler=local --profile=/tmp/leg-<X>.gz`.
   Only Bsc actions re-execute (~1,600 of them).

## Analysis

The profiles are gzipped Chrome traces. Per-action rows are events with
`ph == 'X'`, `cat == 'action processing'`, and `args.mnemonic` in
`{BscV, BscBa}`; `name` is the output path and `dur` is microseconds.
Join by `name` across legs (a leg interrupted and resumed produces two
profiles — load both, later wins). `util/analyze-prim-legs.py` in this
repo does the join:

```
util/analyze-prim-legs.py /tmp/leg-A.gz /tmp/leg-B.gz
```

Report the sum over common actions per leg, the faster/slower counts,
and the top per-module deltas both ways.

## The memory witness

`//rtl/tile:TileSSInputsPorts` is the heaviest known elaboration in the
tree: on a 15GB machine at `--jobs=4` it was OOM-killed under both the
pinned production compiler and compiler A. Nothing else in the
`bs_library` set depends on it, so measure it isolated, one compiler at
a time:

```
/usr/bin/time -v bazel build //rtl/tile:TileSSInputsPorts \
    --//third_party/bluespec:compiler=local
```

(with a `bazel shutdown` + rebuild between compilers, or just flip
`MATX_BSC_LOCAL`). Peak-RSS per compiler on this target is the single
most decisive number.

## Semantic watch-list under B

- Corecursive lists knotted through `List.map` (the old
  `evens = 0 :> map inc odds` idiom) now exhaust the unfolding steps
  budget (G0024) instead of elaborating lazily.
- `[i]` / `!!` on an *infinite* list steps out (the checked select now
  computes the full length first); zipWith against an infinite second
  list still works.
- Any target that fails under B but passes under A is a finding —
  report it with the error tag.
