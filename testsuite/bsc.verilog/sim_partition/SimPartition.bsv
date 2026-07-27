// "watched" is read only by a $display, so nothing synthesizable reaches
// it; "counted" feeds the module's own output and must stay visible.
interface SimPartition;
   method Bit#(8) result;
endinterface

(* synthesize *)
module mkSimPartition (SimPartition);
   Reg#(Bit#(8)) counted <- mkReg(0);
   Reg#(Bit#(8)) watched <- mkReg(0);

   rule go;
      counted <= counted + 1;
      watched <= counted;
   endrule

   rule show;
      // computed only to be printed
      Bit#(8) scaled = (watched << 2) ^ 8'hA5;
      $display("%d", scaled);
   endrule

   method result = counted;
endmodule
