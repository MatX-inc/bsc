// The stable counterpart of DynArgWire: the submodule Port argument
// is a pure function of register reads and constants, so it cannot
// change within a cycle and atomic semantics hold vacuously -- no
// warning.

(* synthesize *)
module mkDynArgRegSub#(Bit#(8) v)(Empty);
   Reg#(Bit#(8)) r <- mkReg(0);
   rule track;
      r <= v;
   endrule
endmodule

(* synthesize *)
module sysDynArgReg(Empty);
   Reg#(Bit#(8)) x <- mkReg(0);
   Reg#(Bit#(8)) y <- mkReg(1);

   Empty sub <- mkDynArgRegSub((x + y) ^ 8'h5A);
endmodule
