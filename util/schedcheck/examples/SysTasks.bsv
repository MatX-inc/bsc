// System tasks inside rules.
//  * first_r and second_r fire in the same cycle and both print; the output
//    order is the execution order (first_r writes note, second_r reads it)
//  * second_r prints conditionally on a value forwarded through an RWire
//    in the same cycle, and calls $finish under a condition
//  * AState turns each task into an AForeignCall whose first argument is
//    WILL_FIRE_<rule> && <condition>
//
// Checked (bsc 2026.07-6): order first_r, second_r; both always fire.
//
// Compile: bsc -u -verilog -elab -show-schedule SysTasks.bsv
(* synthesize *)
module mkSysTasks(Empty);
   Reg#(Bit#(8))   cyc  <- mkReg(0);
   RWire#(Bit#(8)) note <- mkRWire;

   rule first_r;
      note.wset(cyc * 2);
      $display("first: cyc=%0d", cyc);
   endrule

   rule second_r;
      cyc <= cyc + 1;
      if (note.wget matches tagged Valid .v)
         $display("second: note=%0d", v);
      $write("tick\n");
      if (cyc == 5)
         $finish(0);
   endrule
endmodule
