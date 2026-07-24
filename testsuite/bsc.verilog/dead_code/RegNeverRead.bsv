// A register that is written but whose value nothing reads -- not any
// def, instance argument, system task, or output.  The state it holds is
// dead.  The write side is live, so this is the dual of
// RegNeverWritten.bsv.

(* synthesize *)
module mkRegNeverRead (Empty);

   Reg#(Bit#(8)) never_read <- mkReg(0);
   Reg#(Bit#(8)) counter <- mkReg(0);

   rule bump;
      counter <= counter + 1;
      never_read <= counter;
   endrule

   rule show;
      $display("%0d", counter);
   endrule

endmodule
