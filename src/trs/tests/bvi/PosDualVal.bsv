// R3 battery (v1.5.1): TWO imports of ONE Verilog module with
// DIFFERENT literal parameter values -- same interface shape (same
// contract hash), distinct classes and distinct run keys.  Pins the
// dedup discipline: build_all and the trs run precheck must key on the
// full run identity, never on the module name or the bare contract
// JSON, or the second valuation is silently skipped at build (and the
// load-only run then cold-errors -- or worse, finds the wrong model).
interface Ifc;
   method Action go();
endinterface

import "BVI" ParamShow =
module mkParamShow1#(Integer sp)(Ifc);
   parameter SIGNED_P = sp;
   default_clock clk(CLK);
   default_reset rst(RST_N);
   method go() enable(GO);
   schedule go C go;
endmodule

(* synthesize *)
module sysPosDualVal();
   let a <- mkParamShow1(-5);
   let b <- mkParamShow1(7);
   rule fire;
      a.go();
      b.go();
   endrule
endmodule
