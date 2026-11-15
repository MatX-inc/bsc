import GetPut::*;
import FIFO::*;

(* synthesize *)
module sysHWPeekNames ();
   FIFO#(Bit#(8)) f <- mkFIFO;
   Reg#(Bit#(8)) r <- mkReg(0);
   Reg#(Bit#(8)) s <- mkReg(0);
   Get#(Bit#(8)) g = toGet(f);

   // peekGet of an ActionValue of hardware only (a FIFO): the names of the
   // temporaries here and in the later rule are those of running it
   rule look (peekGet(g) != 3);
      let y = peekGet(g) + r;
      $display("y=%0d %0d", y, y + 1);
      s <= y * y;
   endrule

   rule later;
      Bit#(8) z = r + s;
      $display("z=%0d", z);
      $display("z2=%0d", z * 3);
      r <= z;
      f.enq(z);
   endrule
endmodule
