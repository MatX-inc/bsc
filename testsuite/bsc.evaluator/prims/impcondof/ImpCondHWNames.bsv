import FIFO::*;

(* synthesize *)
module sysImpCondHWNames ();
   FIFO#(Bit#(8)) f1 <- mkFIFO;
   FIFO#(Bool) f2 <- mkFIFO;
   FIFO#(Bool) f3 <- mkFIFO;
   Reg#(Bit#(8)) r <- mkReg(0);
   Reg#(Bit#(8)) s <- mkReg(0);

   // impCondOf of an Action of hardware only (FIFOs): the names of the
   // temporaries here and in the later rule are those of running it
   Action a = action
         let y = f1.first + r;
         if (y == 0) f2.enq(True); else f3.enq(False);
      endaction;

   rule look;
      $display("cond a = %d", impCondOf(a));
      let y = f1.first + r;
      $display("y=%0d %0d", y, y + 1);
      s <= y * y;
   endrule

   rule later;
      Bit#(8) z = r + s;
      $display("z=%0d %0d", z, z * 3);
      r <= z;
   endrule
endmodule
