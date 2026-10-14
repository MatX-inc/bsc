typedef struct {
   Bit#(8) first;
   Bit#(8) second;
} Sample deriving (Bits, Eq);

interface Leaf#(type a);
   method a read();
   method Action write(a x);
   method ActionValue#(a) exchange(a x);
endinterface

interface Tree;
   interface Leaf#(Bit#(8)) child;
   method Sample sample();
endinterface

// Partial updates need the generic helpers to create interface structure
// without forcing its untouched undefined or uninitialized methods.
function Bit#(8) fromUndefined();
   Tree t = ?;
   t.child.read = 13;
   return t.child.read;
endfunction

function Bit#(8) fromUninitialized();
   Tree t;
   t.child.read = 17;
   return t.child.read;
endfunction

(* synthesize *)
module mkTree(Tree);
   Reg#(Bit#(8)) value <- mkReg(5);
   interface Leaf child;
      method read = value;
      method Action write(Bit#(8) x);
         value <= x;
      endmethod
      method ActionValue#(Bit#(8)) exchange(Bit#(8) x);
         value <= x;
         return value;
      endmethod
   endinterface
   method sample = Sample { first: value, second: value + 1 };
endmodule
