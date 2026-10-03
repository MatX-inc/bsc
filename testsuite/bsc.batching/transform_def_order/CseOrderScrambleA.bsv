package CseOrderScrambleA;

interface CseOrderScrambleA;
   method Action put(Bit#(8) aa, Bit#(8) ab);
endinterface

// Mentions only the first pair of argument names, aa and ab, so compiling
// it interns put_aa and put_ab and not put_zz and put_zy.
(* synthesize *)
module mkCseOrderScrambleA(CseOrderScrambleA);
   Reg#(Bit#(8)) r1 <- mkReg(0);
   method Action put(Bit#(8) aa, Bit#(8) ab);
      r1 <= aa + ab;
   endmethod
endmodule

endpackage
