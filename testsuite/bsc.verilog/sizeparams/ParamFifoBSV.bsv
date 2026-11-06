package ParamFifoBSV;

interface ParamFIFOL1#(numeric type width);
   method Action enq(Bit#(width) x);
   method Action deq();
   method Bit#(width) first();
   method Action clear();
   method Bool notEmpty();
endinterface

// Pipeline 1-element parameterized FIFO (FIFOL1 style):
// deq < enq, allowing simultaneous deq and enq when already full.
(* synthesize *)
module mkParamFIFOL1 (ParamFIFOL1#(width))
   provisos (Add#(1, _w, width));

   Reg#(Bit#(width)) d0      <- mkReg(0);
   Reg#(Bool)        full    <- mkReg(False);
   RWire#(Bit#(width)) rwEnq <- mkRWire;
   PulseWire         pwDeq   <- mkPulseWire;
   PulseWire         pwClear <- mkPulseWire;

   (* fire_when_enabled, no_implicit_conditions *)
   rule canon;
      if (pwClear) begin
         full <= False;
      end else if (rwEnq.wget matches tagged Valid .v) begin
         d0   <= v;
         full <= True;
      end else if (pwDeq) begin
         full <= False;
      end
   endrule

   method Action enq(Bit#(width) x) if (!full || pwDeq);
      rwEnq.wset(x);
   endmethod

   method Action deq() if (full);
      pwDeq.send();
   endmethod

   method Bit#(width) first() if (full);
      return d0;
   endmethod

   method Action clear();
      pwClear.send();
   endmethod

   method Bool notEmpty() = full;
endmodule

(* synthesize *)
module sysParamFifoBSV (Empty);
   ParamFIFOL1#(4)  q4  <- mkParamFIFOL1;
   ParamFIFOL1#(19) q19 <- mkParamFIFOL1;
   ParamFIFOL1#(72) q72 <- mkParamFIFOL1;
   Reg#(Bit#(4))    step <- mkReg(0);

   rule r0 (step == 0);
      q4.enq(4'ha);
      q19.enq(19'h5a5a5);
      q72.enq(72'hff_11223344_55667788);
      step <= 1;
   endrule

   // Simultaneous deq + enq across two rules while full (pipeline behavior)
   rule r1_deq (step == 1);
      $display("r1 first=%h,%h,%h", q4.first, q19.first, q72.first);
      q4.deq();
      q19.deq();
      q72.deq();
   endrule

   rule r1_enq (step == 1);
      q4.enq(4'h5);
      q19.enq(19'h25a5a);
      q72.enq(72'haa_88776655_44332211);
      step <= 2;
   endrule

   rule r2 (step == 2);
      $display("r2 first=%h,%h,%h", q4.first, q19.first, q72.first);
      q4.deq();
      q19.clear();
      q72.deq();
      step <= 3;
   endrule

   rule r3 (step == 3);
      $display("r3 empty=%b,%b,%b", !q4.notEmpty, !q19.notEmpty, !q72.notEmpty);
      $finish(0);
   endrule
endmodule

endpackage
