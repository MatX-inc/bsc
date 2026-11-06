package ShiftRegBSV;

interface ShiftIfc#(numeric type w);
   method Action shift(Bit#(w) x);
   method Bit#(w) head();
   method Bit#(w) tail();
   method Bit#(16) totalBits();
endinterface

(* synthesize *)
module mkShiftReg#(parameter Bit#(w) init_val)(ShiftIfc#(w));
   Reg#(Bit#(w)) stage0 <- mkReg(init_val);
   Reg#(Bit#(w)) stage1 <- mkReg(init_val);

   method Action shift(Bit#(w) x);
      stage0 <= x;
      stage1 <= stage0;
   endmethod

   method Bit#(w) head();
      return stage0;
   endmethod

   method Bit#(w) tail();
      return stage1;
   endmethod

   method Bit#(16) totalBits();
      return fromInteger(2 * valueOf(w));
   endmethod
endmodule

endpackage
