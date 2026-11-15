import FIFO::*;
import Vector::*;

(* synthesize *)
module sysActionForeignActionValueWorlds();

   FIFO#(File) f1 <- mkFIFO;
   FIFO#(Bool) f2 <- mkFIFO;
   FIFO#(Bool) f3 <- mkFIFO;
   Reg#(Bool) c <- mkReg(True);
   Reg#(Bit#(1)) i <- mkReg(0);
   Vector#(2, FIFO#(Bool)) fs <- replicateM(mkFIFO);
   Vector#(2, Reg#(Bool)) rs <- replicateM(mkReg(False));
   Action a_undet = ?;

   // $fgetc follows actions that a dynamic index chooses (a FIFO enq, whose
   // condition is kept, and a register write) ...
   Action d =
      action
         fs[i].enq(True);
         rs[i] <= True;
         let x <- $fgetc(f1.first);
         if (x == 0) f2.enq(True); else f3.enq(False);
      endaction;

   // ... or an undetermined action in an if: in each, impCondOf does not
   // run it
   Action u =
      action
         if (c) a_undet;
         let x <- $fgetc(f1.first);
         if (x == 0) f2.enq(True); else f3.enq(False);
      endaction;

   rule r;
      $display("cond d = %d", impCondOf(d));
      $display("cond u = %d", impCondOf(u));
   endrule

endmodule
