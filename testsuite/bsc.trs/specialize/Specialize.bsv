// Per-fragment compilation, end to end: one synthesized fragment
// instantiated twice at different parameters is two SPECIALIZATIONS,
// each of which must build ALONE and be byte-identical to what the
// design build produces -- otherwise the object is not the unit it
// claims to be and a build graph over fragments cannot reuse it.
//
// `put' takes an argument and is always_enabled on purpose.  Linking
// a fragment used to hit the top-level binding rule and demand a
// constant for every interface-method argument; there is no answer to
// that demand, and baking one would specialise an object that exists
// to be shared.  `--fragment' is what keeps those ports dynamic, and
// this is the shape that needs it -- an always_enabled method
// argument at a synthesis boundary is the common idiom, not an
// exception, so a regression here would be quiet and widespread.
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
module sysSpecialize();
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
