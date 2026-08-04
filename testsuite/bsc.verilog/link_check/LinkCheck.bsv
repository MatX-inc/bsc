// A registered submodule (no combinational input-to-output paths --
// hierarchical-verilation eligible) and a combinational one (a path
// from its argument to its result -- ineligible), under one top.

interface LSub;
   method Action push(Bit#(8) x);
   method Bit#(8) total();
endinterface

(* synthesize *)
module mkLSub(LSub);
   Reg#(Bit#(8)) acc <- mkReg(0);
   method Action push(Bit#(8) x);
      acc <= acc + x;
   endmethod
   method total = acc;
endmodule

interface LComb;
   method Bit#(8) twice(Bit#(8) x);
endinterface

(* synthesize *)
module mkLComb(LComb);
   method twice(x) = x + x;
endmodule

(* synthesize *)
module sysLinkCheck(Empty);
   LSub  s <- mkLSub;
   LComb c <- mkLComb;
   Reg#(Bit#(8)) n <- mkReg(0);
   rule step;
      s.push(c.twice(n));
      n <= n + 1;
      if (n > 10) $finish(0);
   endrule
endmodule
