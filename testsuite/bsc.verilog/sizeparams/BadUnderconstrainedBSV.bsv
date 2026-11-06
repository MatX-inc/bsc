package BadUnderconstrainedBSV;

(* synthesize *)
module mkBadUnderconstrainedBSV(Reg#(Bit#(w))) provisos (Mul#(k1, k2, w));
   Reg#(Bit#(w)) r <- mkReg(0);
   method Action _write(Bit#(w) x);
      r <= x + fromInteger(valueOf(k1) + valueOf(k2));
   endmethod
   method Bit#(w) _read();
      return r;
   endmethod
endmodule

endpackage
