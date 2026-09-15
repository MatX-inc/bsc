import FIFO::*;

interface SubIfc;
   method Bit#(4) val();
endinterface

(* synthesize *)
module mkVcdGT_Sub (SubIfc);
   Reg#(Bit#(4)) subreg <- mkReg(0);

   rule bump;
      subreg <= subreg + 1;
   endrule

   method Bit#(4) val();
      return subreg;
   endmethod
endmodule

(* synthesize *)
module sysVcdGT ();
   Reg#(UInt#(8))  cyc  <- mkReg(0);
   Reg#(Bit#(16))  wide <- mkRegU;
   RWire#(Bit#(8)) rw   <- mkRWire;
   FIFO#(Bit#(8))  fifo <- mkFIFO;
   SubIfc          sub  <- mkVcdGT_Sub;

   rule count;
      cyc <= cyc + 1;
      wide <= zeroExtend(pack(cyc)) * 3;
   endrule

   rule putwire (pack(cyc)[0] == 0);
      rw.wset(pack(cyc) + 8'h10);
   endrule

   rule enq (pack(cyc)[0] == 0);
      fifo.enq(pack(cyc));
   endrule

   rule deq (pack(cyc)[0] == 1);
      $display("deq %0d", fifo.first);
      fifo.deq;
   endrule

   rule show;
      $display("cycle %0d: wide=%h sub=%h rw_valid=%b",
               cyc, wide, sub.val, isValid(rw.wget));
   endrule

   rule done (cyc == 10);
      $finish(0);
   endrule
endmodule
