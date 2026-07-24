// A register that no rule or method ever writes.  Its enable resolves to
// the constant 0, which is the evidence: downstream the enable pin is
// just tied low, and that is indistinguishable from a write the parent
// gates off.  The value is read, so this is not dead state -- only the
// write side is missing.

(* synthesize *)
module mkRegNeverWritten (Empty);

   Reg#(Bit#(8)) never_written <- mkReg(7);
   Reg#(Bit#(8)) counter <- mkReg(0);

   rule bump;
      counter <= counter + never_written;
   endrule

endmodule
