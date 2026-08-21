// A submodule Port argument fed from a wire read: the value changes
// during the cycle (whichever rule wrote the RWire), but the
// generated Verilog wires the argument combinationally with no
// scheduling relationship to the writer -- the submodule observes
// the settled end-of-cycle value regardless of rule order, outside
// the atomic rule semantics.  The -verilog flow warns (G0129).

(* synthesize *)
module mkDynArgWireSub#(Bit#(8) v)(Empty);
   Reg#(Bit#(8)) r <- mkReg(0);
   rule track;
      r <= v;
   endrule
endmodule

(* synthesize *)
module sysDynArgWire(Empty);
   RWire#(Bit#(8)) w <- mkRWire;
   Reg#(Bit#(8)) x <- mkReg(0);

   rule setw;
      w.wset(x + 1);
   endrule

   Empty sub <- mkDynArgWireSub(fromMaybe(0, w.wget));
endmodule
