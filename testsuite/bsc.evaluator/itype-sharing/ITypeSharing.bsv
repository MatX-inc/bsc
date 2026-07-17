// Evaluator-side types must be handled as the shared DAGs that IType
// interning makes them, never re-traversed per use as trees.
//
// The let chain below doubles a tuple type 18 times: v18's type has
// 2^18 leaf occurrences when unrolled as a tree, but as an interned
// DAG it is 18 ITAp layers -- construction, comparison, and metadata
// reads are all cheap.  The replicate then creates `CELLS evaluator
// heap cells whose types all reference that shared type (the
// harness compiles this file twice: with a small `CELLS as a same-
// machine calibration reference, and with a large `CELLS as the
// stress instance -- see itype-sharing.exp).
//
// Per heap cell the evaluator (a) substitutes into and (b) normalizes
// the cell's type, and (c) deep-forces the IRefT it builds around it.
// Each of those is O(1) on top of the interned node metadata (cached
// free-variable sets, the ATF-free bit, WHNF-only rnf).  If any one
// of them walks the type structurally instead, it repeats work per
// unrolled tree node: CELLS x 2^18 nodes here, tens of CPU-minutes
// to CPU-hours at the stress size -- and the same regression on a
// design with deeper type nesting has no finite compile time at all.
// The doubling makes the boundary qualitative: sharing-aware
// compiles cost barely more than the calibration reference, one
// reintroduced walk cannot come close.

`ifndef CELLS
`define CELLS 65536
`endif

import List::*;

(* synthesize *)
module sysITypeSharing(Empty);

   let v0 = 1'b0;
   let v1 = tuple2(v0, v0);
   let v2 = tuple2(v1, v1);
   let v3 = tuple2(v2, v2);
   let v4 = tuple2(v3, v3);
   let v5 = tuple2(v4, v4);
   let v6 = tuple2(v5, v5);
   let v7 = tuple2(v6, v6);
   let v8 = tuple2(v7, v7);
   let v9 = tuple2(v8, v8);
   let v10 = tuple2(v9, v9);
   let v11 = tuple2(v10, v10);
   let v12 = tuple2(v11, v11);
   let v13 = tuple2(v12, v12);
   let v14 = tuple2(v13, v13);
   let v15 = tuple2(v14, v14);
   let v16 = tuple2(v15, v15);
   let v17 = tuple2(v16, v16);
   let v18 = tuple2(v17, v17);

   let xs = List::replicate(`CELLS, v18);
   Integer n = List::length(xs);

   rule r;
      $display(n);
      $finish(0);
   endrule

endmodule
