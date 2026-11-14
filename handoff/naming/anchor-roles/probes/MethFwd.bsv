package MethFwd;
import SubIfc::*;
// Synthesized module whose methods forward to a synthesized submodule.
//  RDY_put == s$RDY_put == CAN_FIRE_put, EN_put == s$EN_put == WILL_FIRE_put,
//  s$put_v == put_v, get == s$get, RDY_get == 1.
//  cput: method condition on the call (for -keep-method-conds),
//  s2$EN_put == EN_cput && cput_v != 0
//  inl : forwards to an inlined submodule (no EN/RDY signals for it)
interface Fwd;
   method Action put(Bit#(8) v);
   method Bit#(8) get;
   method ActionValue#(Bit#(8)) take;
   method Action cput(Bit#(8) v);
   method Action iput(Bit#(8) v);
   method Bit#(8) iget;
endinterface

(* synthesize *)
module sysMethFwd(Fwd);
   SubIfc s  <- mkSub;
   SubIfc s2 <- mkSub;
   SubIfc si <- mkSubI;
   method put = s.put;
   method get = s.get;
   method take = s.take;
   method Action cput(Bit#(8) v);
      if (v != 0) s2.put(v);
   endmethod
   method iput = si.put;
   method iget = si.get;
endmodule
endpackage
