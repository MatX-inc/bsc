import BDPIActionValueImport::*;

// A converted ActionValue_ that two rules run.  toActionValue_ makes the
// call where it converts the action, and both rules share that one call:
// the Verilog makes the call once and both rules read its result, while
// the Bluesim code makes a call for every execution, so the two disagree
// (a and b read the same value in the Verilog and different values in
// Bluesim).  Nothing reports this.
//
// This is a reproducer for that open case.  foreign.exp does not run it,
// because a test that compares the two simulations fails.
(* synthesize *)
module mkBDPIActionValueConvertRules ();
   Reg#(Bit#(32)) ra <- mkReg(0);
   Reg#(Bit#(32)) rb <- mkReg(0);
   Reg#(Bit#(8)) cyc <- mkReg(0);

   let av_ = toActionValue_(my_count(1));

   rule a;
      let x <- fromActionValue_(av_);
      ra <= x;
   endrule

   rule b;
      let y <- fromActionValue_(av_);
      rb <= y;
   endrule

   rule show;
      $display("cycle %0d: a %0d b %0d", cyc, ra, rb);
      cyc <= cyc + 1;
      if (cyc == 3) $finish(0);
   endrule
endmodule
