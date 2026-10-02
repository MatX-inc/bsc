package EnableOrderScrambleB;

interface EnableOrderScrambleB;
   method Action w8(UInt#(8) x);
   method Action w2(UInt#(8) x);
endinterface

// The probe with w1 renamed: the same shape, so w2's names are the same
// strings as the probe's, interned here first.
(* synthesize *)
module mkEnableOrderScrambleB(EnableOrderScrambleB);
   Reg#(UInt#(8)) r <- mkReg(0);
   method Action w8(UInt#(8) x); r <= x; endmethod
   method Action w2(UInt#(8) x); r <= x; endmethod
endmodule

endpackage
