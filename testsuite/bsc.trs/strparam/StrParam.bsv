// A String parameter must not stop a module being compiled as a
// fragment.
//
// The model says a parent-supplied value reaches a module's body
// through the instance's arena rather than through the code, so one
// object serves every valuation -- bsc.trs/fragobj pins that for a
// Bit parameter.  A String parameter got none of it:
//
//     trs link --fragment mkNamedLeaf.bir
//     thread 'main' panicked: StringConcat of a non-string value
//
// --fragment keeps the parameter dynamic, as it intends, and a
// StringConcat that consumed it then found no string behind the port.
//
// WHERE the concatenation happens is what decides it.  Concatenated
// inside a rule the fragment linked fine, because a link runs no
// rules.  It has to be forced during ELABORATION -- here by building
// a load filename, exactly as the real memory modules build a .mem
// name (cf. Grid.bs: `memName + "_" + sx + "x" + sy').
//
// Not a corner: every memory module in a real design carries such a
// parameter, so the modules that could not be shared were the ones
// instantiated everywhere, and each design built on them fell back to
// compiling itself whole.  A sibling family parameterized by String
// that never concatenates it linked and compiled fine, which is what
// made this look narrow while costing the most.
//
// An unsupplied String now contributes nothing to a concatenation
// instead of panicking, which is sound because the result cannot
// reach the object: the only elaboration-time consumer of a computed
// string is a mem-file prim's load filename, and a link opens none --
// a `.mem' is an input to the simulation, not to the build.  The .exp
// gates exactly that, by requiring the object built knowing NO
// filename to be byte-identical to the one the design built knowing
// `a.hex', and the design to still load a.hex and get the right
// answers.
import RegFile::*;
interface Ifc;
   (* always_ready, always_enabled *) method Action put(Bit#(8) v);
   method Bit#(8) get();
endinterface

(* synthesize *)
module mkNamedLeaf#(parameter String nm)(Ifc);
   // the concatenation happens during ELABORATION, as it does in real
   // memory modules, which build a .mem filename from their name
   RegFile#(Bit#(4), Bit#(8)) rf <- mkRegFileLoad(nm + ".hex", 0, 15);
   Reg#(Bit#(8)) r <- mkReg(0);

   method Action put(Bit#(8) v); r <= rf.sub(truncate(v)); endmethod
   method Bit#(8) get() = r;
endmodule

(* synthesize *)
module sysStrParam();
   Ifc a <- mkNamedLeaf("a");
   Reg#(Bit#(8)) n <- mkReg(0);
   rule go;
      n <= n + 1;
      a.put(n);
      $display("a=%0d", a.get());
      if (n == 3) $finish(0);
   endrule
endmodule
