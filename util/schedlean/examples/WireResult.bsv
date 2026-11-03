interface WireResult;
   method Bit#(4) get;
   method Action put(Bit#(4) x);
endinterface

// A value method whose result is a datapath operation on a wire that sees
// same-cycle calls: get adds r to what put sends through w this cycle.  The
// sequential semantics computes the sum from what get observes, the hardware
// from its own wires; a congruence ties the two applications of the adder.
// The schedule exports get C put, so the parent never calls both in a cycle.
//
// Compile: bsc -u -verilog -elab -show-schedule WireResult.bsv
(* synthesize *)
module mkWireResult(WireResult);
   Reg#(Bit#(4))   r <- mkReg(0);
   RWire#(Bit#(4)) w <- mkRWire;

   method Bit#(4) get = r + fromMaybe(0, w.wget);

   method Action put(Bit#(4) x);
      r <= x;
      w.wset(x);
   endmethod
endmodule
