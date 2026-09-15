# Boundary contract: measured requirements for the interface refactor

Self-contained handoff (2026-07-12) from the Bazel-friendliness audit
to the bsc interface-contract refactor.  Everything here is measured
on this tree (bsc f2967c80-era installed binary) and reproducible with
`src/trs/tools/ba-stability-audit.sh`.  Context lives in
docs/CPHASE-PLAN.md, but this file stands alone.

## The requirement, in one sentence

A `(* synthesize *)` boundary should be a true compilation boundary:
a parent package's compiled artifacts (`.bo`, `.ba`) must depend on a
child's INTERFACE CONTRACT only — so a child BODY edit leaves the
parent's bytes untouched.

## What the contract must contain

Everything a parent needs to type-check, elaborate, and SCHEDULE
against the child — which is richer than types:

- interface types and method signatures,
- port shapes (argument/result widths, enables, readies),
- the method scheduling annotations (conflict matrix: CF/SB/SBR/C
  between methods),
- clock and reset domain relationships (which ifc clocks, gating),
- path info (combinational input-to-output paths).

This is exactly the information `VModInfo` carries and what
`import "BVI"` consumes — the precedent that such a contract is
sufficient: BVI imports elaborate and schedule with no body at all,
and the Verilog backend codegens at `-c` on the same basis (children
referenced by name).  The contract artifact is "the auto-derived
BVI-equivalent of a synthesized module."

## The two mechanisms that break it today (measured)

Experiment: grid v3 N=8 split into Tile.bsv (leaf `mkTile`,
synthesized) + Grid8.bsv (spine `sysGrid8`).  Edit ONE constant in
the leaf BODY (same byte length: 9e3779b9 -> 9e3779ba), recompile,
byte-compare the PARENT's artifacts.

1. ELABORATION-POSITION NAMING.  `sysGrid8.ba` changes wholesale:
   12,556 changed dumpba lines — the parent's OWN defs are renamed
   and reordered, e.g.

       2547,2549c2547,4148
       <   t57_oTake_009_BITS_11_TO_0___d2027 :: Bit 12;
       ...
       >   cyc_PLUS_1___d2 :: Bit 32;
       >   cs__h15389 :: Bit 32;
       >   cs__h15389  = cs__h15388 ^ accVal__h15328;

   The `__h<N>`/`__d<N>` suffixes are evaluator heap positions.  The
   parent's elaboration walks the child's ISyntax even across the
   synthesize boundary, so any child-body perturbation shifts the
   parent's numbering, which renames defs, which reorders the
   serialized (Id-keyed) maps.  Requirement: either the parent's
   evaluator consumes a boundary STUB (never walks the child body),
   or def naming/numbering becomes position-independent
   (upstream main's Bluesim-codegen canonicalization series,
   c04e746b/2fb5260f/1839fe82/4eda94e0, is the adjacent precedent).

2. FULL-CONTENT DEP HASHES.  A parent `.bo` embeds, per import, a
   content hash over the importee's ENTIRE `.bo` — bi_sig, bo_sig,
   AND the IPackage ISyntax (`ipkg_depends`; read via decodeWithHash,
   BinUtil.hs:184).  Any child edit therefore changes the parent's
   `.bo` bytes even where naming would survive.  Requirement: scope
   the hash to what the importer can actually consume.  For
   synthesized children that is the contract above; for
   non-synthesized imports the full-content hash is CORRECT (bodies
   are inlined — implementation IS the interface below the
   boundary), so the hash domain must split at the same boundary
   the stub does.

## What is already solved (do not re-solve)

- Same-directory determinism: PASSES today — .bo/.ba are
  byte-identical across in-place recompiles given
  `-no-show-timestamps -no-show-version` (both load-bearing).
- Path sensitivity: PR #1040 (`-remap-path-prefix FROM=TO`,
  serialization-time remap at the BinData share chokepoint +
  abmi_path/src_name + stored-Flags normalization) makes .bo/.ba
  byte-identical across build directories; no-flag bytes unchanged.
  Its audit also established there are NO timestamps in either
  format.

## Acceptance test

`src/trs/tools/ba-stability-audit.sh` runs three checks; C is this
refactor's gate:

    A same-dir determinism        : PASS today
    B path insensitivity          : PASS with #1040's flag
    C parent stability under leaf : FAIL today  <- flips green when
      body edit                     the contract is load-bearing

Usage (grid8-split example):

    BSC=inst/bin/bsc src/trs/tools/ba-stability-audit.sh /tmp/audit \
        Tile.bsv Grid8.bsv sysGrid8 's/9e3779b9/9e3779ba/' \
        -remap-path-prefix "$PWD=."

## Why it pays (the consumers)

1. Build systems (Bazel or make): a parent action declares the
   child's CONTRACT artifact as input — child body edits stop
   rerunning parents entirely (the ijar/hjar pattern), and the
   measured ~0%-reuse-on-leaf-edits of Bluesim's object cache is
   fixed by the same stroke.
2. Boundary-stub elaboration: the parent evaluator consumes the
   contract; heap-position independence falls out.
3. Interface-scoped hashing: `.bo` stamps stop cascading.
4. trs: the per-module BIR fragment identity (content_hash /
   subtree_hash, SimExportIR.hs:733 TODO) wants the same boundary.
   Caveat: where trs DELIBERATELY crosses the boundary (fused exec
   bodies inline child method bodies for speed), closure keys — not
   stubs — remain correct; the contract does not replace them.
