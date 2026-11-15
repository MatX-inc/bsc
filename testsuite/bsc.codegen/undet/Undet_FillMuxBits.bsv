// Under -split-undetermined-vals, undetermined bits in one arm of an
// if-expression take the value of the same bits in the other arm, so that
// those bits leave the mux.  By default they keep the -unspecified-to value.

interface FillMuxBits;
   method Bit#(5) out1(Bit#(5) x, Bool c);
endinterface

(* synthesize *)
module mkUndet_FillMuxBits(FillMuxBits);
   // the low bits of the first arm come from the second arm
   method Bit#(5) out1(Bit#(5) x, Bool c) = c ? {3'b001, ?} : x;
endmodule

// Bluesim and Verilog pick the same values
(* synthesize *)
module sysUndet_FillMuxBits(Empty);
   FillMuxBits dut <- mkUndet_FillMuxBits;
   Reg#(Bit#(6)) i <- mkReg(0);
   rule step;
      Bit#(5) x = i[4:0];
      Bool c = i[5] == 1;
      $display("%b %b %b", x, pack(c), dut.out1(x, c));
      if (i == 63) $finish(0);
      i <= i + 1;
   endrule
endmodule
