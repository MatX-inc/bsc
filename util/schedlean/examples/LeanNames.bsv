interface LeanNames;
   method Bit#(8) init;
   method Action mk(Bit#(8) from);
   method Bool hw;
endinterface

// Names that Lean reserves, or defines itself in a structure's namespace:
// methods init, mk and hw, a register rec, a rule using, a method argument
// from.  The Lean file quotes or primes each so that nothing clashes.
//
// Compile: bsc -u -verilog -elab -show-schedule LeanNames.bsv
(* synthesize *)
module mkLeanNames(LeanNames);
   Reg#(Bit#(8)) rec <- mkReg(0);
   Reg#(Bool) hwReg <- mkReg(False);

   rule using (rec != 0);
      hwReg <= !hwReg;
   endrule

   method Bit#(8) init = rec;

   method Action mk(Bit#(8) from);
      rec <= from;
   endmethod

   method Bool hw = hwReg;
endmodule
