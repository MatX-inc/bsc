// A def created by aOpt: under -opt-if-mux the optimizer turns a
// conditional into a mux and names its condition, a compound
// expression the expansion before it inlined, as a def of its own
// (_dm<n>), created from the def being optimized.
(* synthesize *)
module sysMetaOpt(Empty);
   Reg#(UInt#(8)) a <- mkReg(0);
   Reg#(UInt#(8)) b <- mkReg(0);
   Reg#(UInt#(8)) c <- mkReg(0);
   Reg#(UInt#(8)) d <- mkReg(0);

   rule step;
      a <= ((b > c) && (d != 3)) ? b + d : c - d;
   endrule
endmodule
