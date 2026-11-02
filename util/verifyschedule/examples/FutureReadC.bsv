interface Fake;
   method Action  act(Bit#(8) x);
   method Bit#(8) val;
endinterface

// A value method read before a call of an action method it conflicts with
// (act C val), in an imported child that declares no combinational path
// between them.  A read followed in the cycle by a call it may not precede
// sees an arbitrary value: reader's view of f.val is tied to the
// hardware's only when it sees every call of f.act of the cycle before it.
//  * reader reads f.val when c, and reads y
//  * setter calls f.act when !c, and writes y, so reader executes first
//  * the two uses never happen in one cycle, so bsc lets both rules fire
// No Verilog for FakeMod is needed to produce the .ba.
import "BVI" FakeMod =
module mkFake(Fake);
   default_clock clk(CLK);
   default_reset rst(RST_N);
   method act(ACT_X) enable(EN_ACT);
   method VAL val();
   schedule act C val;
   schedule val CF val;
   schedule act C act;
endmodule

// Checked (bsc 2026.07-13): order reader, setter; both always fire.
//
// Compile: bsc -u -verilog -elab -show-schedule FutureReadC.bsv
(* synthesize *)
module mkFutureReadC(Empty);
   Fake f <- mkFake;
   Reg#(Bool)    c <- mkReg(False);
   Reg#(Bit#(8)) x <- mkReg(0);
   Reg#(Bit#(8)) y <- mkReg(0);
   Reg#(Bit#(8)) z <- mkReg(0);

   rule reader;
      if (c) x <= f.val;
      z <= y;
   endrule

   rule setter;
      if (!c) f.act(5);
      y <= y + 1;
   endrule
endmodule
