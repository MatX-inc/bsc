package CseOrderScrambleB;

interface CseOrderScrambleB;
   method Action put(Bit#(8) zz, Bit#(8) zy);
endinterface

// Mentions only the second pair of argument names, zz and zy, so compiling
// it interns put_zz and put_zy and not put_aa and put_ab.
(* synthesize *)
module mkCseOrderScrambleB(CseOrderScrambleB);
   Reg#(Bit#(8)) r1 <- mkReg(0);
   method Action put(Bit#(8) zz, Bit#(8) zy);
      r1 <= zz + zy;
   endmethod
endmodule

endpackage
