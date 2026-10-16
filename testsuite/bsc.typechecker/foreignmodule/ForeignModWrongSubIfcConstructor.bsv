interface Leaf#(type a);
  method a read();
endinterface

// The same fields do not make these nominal interface types interchangeable.
interface OtherLeaf#(type a);
  method a read();
endinterface

interface Branch#(type a);
  interface Leaf#(a) leaf;
endinterface

interface Ifc#(type a);
  interface Branch#(a) branch;
endinterface

import "BVI" MOD =
module mkWrongSubIfcConstructor (Ifc#(a)) provisos (Bits#(a, n));
  interface Branch branch;
    interface OtherLeaf leaf;
      method Q_OUT read();
    endinterface
  endinterface
  schedule branch.leaf.read CF branch.leaf.read;
endmodule
