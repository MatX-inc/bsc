// Under -split-undetermined-vals, after the fill, a constant arm that the
// condition selects only when the other arm has the same bits (the tag of a
// padded encoding) becomes the other arm.  The conditions of the enclosing
// if-expressions count too, including those of a chain of defs where each
// def is used only as an arm of the next.

interface FillReencode;
   method Bit#(5) cond(Bit#(5) x);
   method Bit#(5) nest(Bit#(5) x, Bit#(5) y, Bool p);
   method Bit#(5) chain(Bit#(5) x, Bit#(5) y, Bit#(5) z, Bool p);
endinterface

(* synthesize *)
module mkUndet_FillReencode(FillReencode);
   // the second arm is chosen only when x[4:2] is 3'b001
   method Bit#(5) cond(Bit#(5) x) =
      (x[4] == 1 || x[4:3] == 2'b01 || x[4:2] == 3'b000) ? x : {3'b001, ?};
   // the inner condition implies that only with the enclosing one
   method Bit#(5) nest(Bit#(5) x, Bit#(5) y, Bool p);
      Bit#(5) d = (x[4:3] != 2'b00) ? y : ((x[2] == 0) ? x : {3'b001, ?});
      return p ? d : y;
   endmethod
   // the same through the defs d1, d2 and d3
   method Bit#(5) chain(Bit#(5) x, Bit#(5) y, Bit#(5) z, Bool p);
      Bit#(5) d3 = (x[2] == 0) ? x : {3'b001, ?};
      Bit#(5) d2 = (x[3] == 1) ? z : d3;
      Bit#(5) d1 = (x[4] == 1) ? y : d2;
      return p ? d1 : y;
   endmethod
endmodule

(* synthesize *)
module sysUndet_FillReencode(Empty);
   FillReencode dut <- mkUndet_FillReencode;
   Reg#(Bit#(6)) i <- mkReg(0);
   rule step;
      Bit#(5) x = i[4:0];
      Bool p = i[5] == 1;
      Bit#(5) y = ~x;
      Bit#(5) z = x ^ 5'b10101;
      $display("%b %b %b %b %b", x, pack(p), dut.cond(x),
               dut.nest(x, y, p), dut.chain(x, y, z, p));
      if (i == 63) $finish(0);
      i <= i + 1;
   endrule
endmodule
