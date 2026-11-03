// A rule split on its condition: the split attribute makes iSplitIf
// create one rule per arm, each from the root rule, which it drops.
interface SplitIfc;
   method UInt#(8) value();
endinterface

(* synthesize *)
module sysMetaSplit(SplitIfc);
   Reg#(UInt#(8)) a <- mkReg(0);
   Reg#(UInt#(8)) b <- mkReg(0);
   Reg#(UInt#(8)) c <- mkReg(0);

   rule pick;
      (* split *)
      if (a > b)
         c <= a;
      else
         c <= b;
   endrule

   method UInt#(8) value();
      return c;
   endmethod
endmodule
