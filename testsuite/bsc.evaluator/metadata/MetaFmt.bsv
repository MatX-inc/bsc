// A formatted display: the Fmt-typed defs of the rule (the format of
// one register, the fshow of another and their concatenation) are
// inlined by iInlineFmt into the display's foreign call, which it then
// splits along the Fmt arguments; the ledger records each inlining.
interface FmtIfc;
   method UInt#(8) value();
endinterface

(* synthesize *)
module sysMetaFmt(FmtIfc);
   Reg#(UInt#(8)) a <- mkReg(0);
   Reg#(UInt#(8)) b <- mkReg(0);

   rule step;
      Fmt f = $format("a=%d", a) + fshow(b);
      $display(f, " sum=%d", a + b);
      a <= a + 1;
   endrule

   method UInt#(8) value();
      return a;
   endmethod
endmodule
