// A shape bsc's own testsuite does not have: the same module
// instantiated repeatedly, several levels down, with calls crossing
// every boundary and a second clock domain reaching the bottom.
//
// The merge is a fold over the instance tree, and everything it reads
// is per module type -- so a design with many instances of few types is
// the one that tells you whether the fold and the reading agree.  It is
// also the shape a real design has.
import Clocks::*;

interface Leaf;
   method Action put(Bit#(16) x);
   method Bit#(16) get();
endinterface

(* synthesize *)
module mkLeaf(Leaf);
   Reg#(Bit#(16)) a <- mkReg(0);
   Reg#(Bit#(16)) b <- mkReg(0);
   rule shift;
      b <= a;
   endrule
   method Action put(Bit#(16) x);
      a <= x + 1;
   endmethod
   method Bit#(16) get() = b;
endmodule

interface Mid;
   method Action drive(Bit#(16) x);
   method Bit#(16) total();
endinterface

// Two leaves, and a rule of its own that calls into both: the parent's
// call has to fuse through this module's method AND its rule.
(* synthesize *)
module mkMid(Mid);
   Leaf l0 <- mkLeaf;
   Leaf l1 <- mkLeaf;
   Reg#(Bit#(16)) seen <- mkReg(0);
   rule collect;
      seen <= l0.get() + l1.get();
   endrule
   method Action drive(Bit#(16) x);
      l0.put(x);
      l1.put(x + 1);
   endmethod
   method Bit#(16) total() = seen;
endmodule

(* synthesize *)
module sysDeepTiles();
   // the same module type twice, so the fold meets it more than once
   Mid m0 <- mkMid;
   Mid m1 <- mkMid;

   // a second domain, generated here, clocking state of its own
   GatedClockIfc gc <- mkGatedClockFromCC(True);
   Reg#(Bit#(16)) slow <- mkReg(0, clocked_by gc.new_clk);

   Reg#(Bit#(16)) cnt <- mkReg(0);

   rule tick;
      cnt <= cnt + 1;
      gc.setGateCond((cnt % 4) < 2);
      m0.drive(cnt);
   endrule

   rule other;
      m1.drive(m0.total());
   endrule

   rule slowly;
      slow <= slow + 1;
   endrule

   rule done (cnt > 20);
      $display("m0 = %0d  m1 = %0d  slow = %0d", m0.total(), m1.total(), slow);
      $finish(0);
   endrule
endmodule
