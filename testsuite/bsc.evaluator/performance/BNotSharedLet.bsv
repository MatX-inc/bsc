package BNotSharedLet;

// BNotShared with each stage's negation bound to a name that is used
// twice, so that the negation's result is a heap cell evaluated on its
// own.  Prelude's not is primChr (primBNot (primOrd x)): primBNot pushes
// through the SHARED conditional DAG of the previous stages once per
// cell (pushBNot), and primChr is then applied to the pushed DAG.  The
// evaluator must keep that DAG as one cell under primChr; pushing the
// coercion into the arms once per path (evalStaticOp) rebuilds it as an
// inline tree of ~2^(2K) leaves and never terminates at K=64.

import Vector :: *;

typedef 16 N;   // vector width  (leaf register cells)
typedef 64 K;   // full odd-even steps (if-depth grows ~2 per step)

function Vector#(n, Bool) oestep (Vector#(n, Bool) vx);
   Vector#(n, Bool) evn = vx;
   for (Integer j = 1; j < valueOf(n); j = j + 2) begin
      Bool nb = !vx[j-1];
      if (nb && vx[j]) begin
         evn[j-1] = vx[j];
         evn[j]   = nb ? vx[j-1] : vx[j-1];   // second use of nb; folds
      end
   end
   Vector#(n, Bool) odd = evn;
   for (Integer j = 2; j < valueOf(n); j = j + 2) begin
      Bool nb = !evn[j-1];
      if (nb && evn[j]) begin
         odd[j-1] = evn[j];
         odd[j]   = nb ? evn[j-1] : evn[j-1];   // second use of nb; folds
      end
   end
   return odd;
endfunction

(* synthesize *)
module sysBNotSharedLet (Empty);
   Vector#(N, Reg#(Bool)) rs <- replicateM(mkRegU);

   rule step;
      Vector#(N, Bool) v = readVReg(rs);
      for (Integer s = 0; s < valueOf(K); s = s + 1)
         v = oestep(v);
      writeVReg(rs, v);
   endrule
endmodule

endpackage
