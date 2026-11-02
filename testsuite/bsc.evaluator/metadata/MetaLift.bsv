// A lifted condition: iLift merges the two writes of one register under
// a condition into one write of a conditional expression; the second
// transform stage makes that expression a def, created from the rule.
interface LiftIfc;
   method UInt#(8) value();
endinterface

(* synthesize *)
module sysMetaLift(LiftIfc);
   Reg#(UInt#(8)) a <- mkReg(0);
   Reg#(UInt#(8)) b <- mkReg(0);
   Reg#(UInt#(8)) c <- mkReg(0);

   rule pick;
      if (a > b)
         c <= a + 1;
      else
         c <= b + 2;
   endrule

   method UInt#(8) value();
      return c;
   endmethod
endmodule
