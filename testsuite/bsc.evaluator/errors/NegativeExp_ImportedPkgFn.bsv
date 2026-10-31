import NegativeExpLib::*;

(* synthesize *)
module sysNegativeExp_ImportedPkgFn();
   rule test;
      $display(pow2(-1));
      $finish(0);
   endrule
endmodule
