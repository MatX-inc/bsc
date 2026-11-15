import FIFO::*;

(* synthesize *)
module sysHWMFixNames ();
   FIFO#(Bit#(32)) f <- mkFIFO;
   Reg#(Bit#(32)) r <- mkReg(0);
   Reg#(Bit#(32)) s <- mkReg(0);

   // mfix of an ActionValue of hardware only (a FIFO): the names of the
   // temporaries here and in the later rule are those of running it
   function ActionValue#(Tuple2#(Bit#(32), Bit#(32))) loop(Tuple2#(Bit#(32), Bit#(32)) x);
      actionvalue
         f.deq;
         return tuple2(f.first, tpl_1(x) + 1);
      endactionvalue
   endfunction

   rule fill;
      f.enq(r);
   endrule

   rule go;
      let v <- mfix(loop);
      let y = tpl_2(v) + s;
      $display("y=%0d", y);
      r <= y * y;
   endrule

   rule later;
      Bit#(32) z = r + s;
      $display("z=%0d %0d", z, z * 3);
      s <= z;
   endrule
endmodule
