// A submodule method whose result nothing consumes.  The wire is dropped
// from the output, so downstream there is no port left to attribute -- and
// which method it belonged to was never in the netlist to begin with, only
// in the interface.

interface Pair;
   method Bit#(8) used_result;
   method Bit#(8) unused_result;
endinterface

(* synthesize *)
module mkPair (Pair);
   Reg#(Bit#(8)) r <- mkReg(3);
   method used_result = r;
   method unused_result = r + 1;
endmodule

(* synthesize *)
module mkMethodResultUnused (Empty);
   Pair p <- mkPair;
   Reg#(Bit#(8)) sink <- mkReg(0);

   rule use_one;
      sink <= p.used_result;
   endrule
endmodule
