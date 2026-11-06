package BadClassProvisoBSV;

typeclass WidthPolicy#(type t);
   function t maskVal(t x);
endtypeclass

(* synthesize *)
module mkBadClassProvisoBSV(Reg#(Bit#(w))) provisos (WidthPolicy#(Bit#(w)));
   Reg#(Bit#(w)) r <- mkReg(0);
   method Action _write(Bit#(w) x);
      r <= x;
   endmethod
   method Bit#(w) _read();
      return r;
   endmethod
endmodule

endpackage
