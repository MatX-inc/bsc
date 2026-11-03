// A dropped rule: the rule whose condition is False never fires and
// iDropRules removes it (with a warning), which the ledger records.
interface DropIfc;
   method UInt#(8) value();
endinterface

(* synthesize *)
module sysMetaDrop(DropIfc);
   Reg#(UInt#(8)) a <- mkReg(0);
   Reg#(UInt#(8)) c <- mkReg(0);

   rule never (False);
      c <= a;
   endrule

   rule step;
      a <= a + 1;
   endrule

   method UInt#(8) value();
      return c;
   endmethod
endmodule
