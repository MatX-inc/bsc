// R5: TWO reset ports in one fragment.  A BVI output reset creates a
// derived reset node inside the fragment's own subtree, alongside the
// default reset, and a register there is reset_by it -- so the
// instance's reset table has two entries rather than the corpus mean
// of 1.02, which is the only shape that can observe their ORDER.
//
// That order came from `HashMap::iter', so it differed per map, and
// the process that EMITTED an object numbered the table differently
// from the one that LOADED it -- a wrong answer, not a miss.
// Ordinals are assigned by port name now.  The order is baked into
// the emitted code, so the gate is on the artifact itself: separate
// compilations must agree byte for byte, and the object built alone
// must equal the one the design wrote.
interface RstStretchIfc;
   method Action go();
   method Bit#(2) state();
   interface Reset rst_out;
endinterface

import "BVI" RstStretch =
module mkRstStretch(RstStretchIfc);
   default_clock clk(CLK);
   default_reset rst(RST_N);
   method go() enable(GO);
   method STATE state();
   output_reset rst_out(RST_OUT) clocked_by(clk);
   schedule state CF state;
   schedule state SB go;
   schedule go C go;
endmodule

interface Wrap;
   method Action go();
   method Bit#(8) held();
   method Bit#(2) st();
endinterface

(* synthesize *)
module mkRstWrap#(Bit#(8) k)(Wrap);
   RstStretchIfc rs <- mkRstStretch;
   Reg#(Bit#(8)) h <- mkReg(0, reset_by rs.rst_out);
   rule bump;
      h <= h + k;
   endrule
   method Action go() = rs.go();
   method Bit#(8) held() = h;
   method Bit#(2) st() = rs.state();
endmodule

(* synthesize *)
module sysPosFragTwoRst();
   Wrap a <- mkRstWrap(1);
   Wrap b <- mkRstWrap(2);
   Reg#(Bit#(8)) n <- mkReg(0);
   rule step;
      n <= n + 1;
      $display("c=%0d a=%0d/%0d b=%0d/%0d", n, a.st(), a.held(), b.st(), b.held());
      if (n == 2) begin a.go(); b.go(); end
      if (n == 12) $finish(0);
   endrule
endmodule
