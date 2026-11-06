package BadForLoopBSV;

(* synthesize *)
module mkBadForLoopBSV(Reg#(Bit#(w)));
   Reg#(Bit#(w)) r <- mkReg(0);

   // Well-typed interface and body, but static loop bound valueOf(w)
   // cannot be unrolled at elaboration time when w is a size parameter.
   method Action _write(Bit#(w) x);
      Bit#(w) acc = x;
      for (Integer i = 0; i < valueOf(w); i = i + 1) begin
         acc = acc + 1;
      end
      r <= acc;
   endmethod

   method Bit#(w) _read();
      return r;
   endmethod
endmodule

endpackage
