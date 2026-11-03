// A system task printing a value chosen by a chain of selections, over
// reads that are out of order where the chain does not select them.
//  * a writes r[i] when x == i, and reads q, which b writes: a runs first
//  * b prints acc, chosen from the r[i] by a chain of selections, one
//    definition each: the last r[i] with y[i] set and x != i.  b reads
//    every r[i], each out of order next to a's write of it, but it prints
//    only one a did not write: legal, which the theorem shows by following
//    the chain.  That takes a definition per link, not one per link and
//    read (`<def>@up`, `<def>@reaches@<copy>`)
//
// Compile: bsc -u -verilog -elab TaskChain.bsv
import Vector::*;

(* synthesize *)
module mkTaskChain(Empty);
   Vector#(6, Reg#(Bit#(8))) r <- replicateM(mkReg(0));
   Reg#(UInt#(3))            x <- mkReg(0);
   Reg#(Bit#(6))             y <- mkReg(0);
   Reg#(Bit#(8))             q <- mkReg(0);

   rule a;
      for (Integer i = 0; i < 6; i = i + 1)
         if (x == fromInteger(i)) r[i] <= q;
   endrule

   rule b;
      q <= q + 1;
      Bit#(8) acc = 0;
      for (Integer i = 5; i >= 0; i = i - 1)
         acc = (y[i] == 1 && x != fromInteger(i)) ? r[i] : acc;
      $display("%d", acc);
   endrule
endmodule
