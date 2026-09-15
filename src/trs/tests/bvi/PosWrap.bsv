// v1.1 battery: a parameter FORWARDED across a synthesis boundary --
// the import sits inside a parameterized wrapper module, so its value
// is not a literal at the import site; it resolves per instantiation
// and each valuation verilates as its own cache class.  Two wrapper
// instances with different valuations pin the per-valuation classing.
interface Ifc;
   method Action go();
endinterface

import "BVI" WrapShow =
module mkWrapShow#(Bit#(8) w, String nm)(Ifc);
   parameter WIDTH_P = w;
   parameter NAME_P = nm;
   default_clock clk(CLK);
   default_reset rst(RST_N);
   method go() enable(GO);
   schedule go C go;
endmodule

(* synthesize *)
module mkWrapper#(parameter Bit#(8) w)(Ifc);
   let inner <- mkWrapShow(w, "wrapped");
   method go = inner.go;
endmodule

(* synthesize *)
module sysPosWrap();
   Ifc a <- mkWrapper(8'd42);
   Ifc b <- mkWrapper(8'd77);
   Reg#(Bit#(2)) st <- mkReg(0);
   rule ra (st == 0);
      a.go();
      st <= 1;
   endrule
   rule rb (st == 1);
      b.go();
      st <= 2;
   endrule
   rule fin (st == 2);
      $finish(0);
   endrule
endmodule
