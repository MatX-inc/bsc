package EnableOrderScrambleA;

interface EnableOrderScrambleA;
   method Action w1(UInt#(8) x);
   method Action w9(UInt#(8) x);
endinterface

// The probe with w2 renamed: the same shape, so w1's names are the same
// strings as the probe's, interned here first.
(* synthesize *)
module mkEnableOrderScrambleA(EnableOrderScrambleA);
   Reg#(UInt#(8)) r <- mkReg(0);
   method Action w1(UInt#(8) x); r <= x; endmethod
   method Action w9(UInt#(8) x); r <= x; endmethod
endmodule

endpackage
