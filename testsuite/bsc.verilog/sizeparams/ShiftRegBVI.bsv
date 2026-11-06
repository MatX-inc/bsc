package ShiftRegBVI;

import ShiftRegBSV::*;

import "BVI" mkShiftReg =
   module mkShiftRegBVI#(Bit#(w) init_val)(ShiftIfc#(w));
      parameter w = valueOf(w);
      parameter init_val = init_val;
      default_clock clk(CLK);
      default_reset rst(RST_N);
      method shift(shift_x) enable(EN_shift);
      method head head();
      method tail tail();
      method totalBits totalBits();
      schedule (head, tail, totalBits) CF (head, tail, totalBits, shift);
      schedule shift SBR shift;
   endmodule

(* synthesize *)
module sysShiftRegBVI(Empty);
   ShiftIfc#(1)  s1  <- mkShiftRegBVI(1'b1);
   ShiftIfc#(12) s12 <- mkShiftRegBVI(12'hABC);
   ShiftIfc#(24) s24 <- mkShiftRegBVI(24'h123456);
   ShiftIfc#(48) s48 <- mkShiftRegBVI(48'hCAFE00001111);

   Reg#(Bit#(8)) cycle <- mkReg(0);

   rule step;
      cycle <= cycle + 1;
      s1.shift(truncate(cycle));
      s12.shift(zeroExtend(cycle));
      s24.shift(zeroExtend(cycle) + 24'h100);
      s48.shift(zeroExtend(cycle) + 48'h200);
      $display("cycle=%0d s1=(%0d,%0d,%0d) s12=(%0h,%0h,%0d) s24=(%0h,%0h,%0d) s48=(%0h,%0h,%0d)",
               cycle,
               s1.head(), s1.tail(), s1.totalBits(),
               s12.head(), s12.tail(), s12.totalBits(),
               s24.head(), s24.tail(), s24.totalBits(),
               s48.head(), s48.tail(), s48.totalBits());
      if (cycle == 3) $finish(0);
   endrule
endmodule

endpackage
