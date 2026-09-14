// Per-fragment compilation, end to end: one .ba, one .bir, one .o.
//
// mkLeaf is instantiated twice at different parameters and must be
// ONE object -- named `mkLeaf.o', with no hash, because there is
// nothing left to disambiguate.  A parent-supplied value lives in a
// slot in the instance's own region, so the object serves every
// valuation and the two instances must still behave differently at
// run time.
//
// The object must also build ALONE, from its own .bir with no
// bindings, and be byte-identical to what the design build produces.
// That is the property the whole model rests on: if it fails, the
// object depends on the design it was compiled in and a build graph
// over fragments cannot reuse it.
//
// `put' takes an argument and is always_enabled on purpose.  Linking
// a fragment used to hit the top-level binding rule and demand a
// constant for every interface-method argument; there is no answer to
// that demand.  `--fragment' keeps those ports dynamic, and an
// always_enabled method argument at a synthesis boundary is the
// common idiom rather than an exception, so a regression here would
// be quiet and widespread.
interface Ifc;
   (* always_ready, always_enabled *) method Action put(Bit#(8) v);
   method Bit#(8) get();
endinterface

(* synthesize *)
module mkLeaf#(Bit#(8) k)(Ifc);
   Reg#(Bit#(8)) r <- mkReg(0);
   method Action put(Bit#(8) v); r <= v + k; endmethod
   method Bit#(8) get() = r;
endmodule

(* synthesize *)
module sysFragObj();
   Ifc a <- mkLeaf(3);
   Ifc b <- mkLeaf(7);
   Reg#(Bit#(8)) n <- mkReg(0);

   rule go;
      n <= n + 1;
      a.put(n);
      b.put(n);
      $display("a=%0d b=%0d", a.get(), b.get());
      if (n == 3) $finish(0);
   endrule
endmodule
