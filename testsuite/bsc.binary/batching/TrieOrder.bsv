package TrieOrder;

// A polymorphic module context: every submodule instantiation leaves an
// IsModule predicate whose module type is still a variable when context
// reduction first sees it, so the instance trie is queried Free and
// enumerates every IsModule instance.  ModuleContext brings a second
// instance, under a different head than Module.
import ModuleContext::*;

interface Ifc;
   method Bool v;
endinterface

module [m] mkLeaf(Ifc) provisos (IsModule#(m, c));
   Reg#(Bool) r <- mkReg(False);
   method Bool v = r;
endmodule

module [m] mkTrieOrder(Ifc) provisos (IsModule#(m, c));
   Ifc a <- mkLeaf;
   Ifc b <- mkLeaf;
   method Bool v = a.v && b.v;
endmodule

endpackage
