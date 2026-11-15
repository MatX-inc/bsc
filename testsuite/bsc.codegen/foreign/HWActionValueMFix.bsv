import FIFO::*;

(* synthesize *)
module sysHWActionValueMFix ();
   FIFO#(Bit#(32)) f <- mkFIFO;
   Reg#(Bit#(32)) r <- mkReg(0);

   // mfix over an ActionValue of hardware only (a FIFO): no foreign call,
   // so no G0129
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
      r <= tpl_2(v);
   endrule
endmodule
