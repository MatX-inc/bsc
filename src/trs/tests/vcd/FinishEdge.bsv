// $finish edge-completion witness: `late` is forced AFTER `fin` on
// the same edge (RWire wset SB wget), so the reference kernel runs
// it post-$finish — its state write LANDS (mark = cyc + 42) while
// its $display is suppressed (dollar_display.cxx bk_finished gate).
// A mid-edge-aborting engine leaves mark = 0: visible in the final
// VCD cycle (vcd ladder) and in interactive peeks (FinishPeek).
(* synthesize *)
module sysFinishEdge();
   Reg#(UInt#(16)) cyc <- mkReg(0);
   Reg#(UInt#(16)) mark <- mkReg(0);
   RWire#(UInt#(16)) w <- mkRWire;

   rule count;
      cyc <= cyc + 1;
      $display("cyc %0d mark %0d", cyc, mark);
   endrule

   rule fin (cyc == 8);
      w.wset(cyc);
      $finish(0);
   endrule

   rule late (w.wget matches tagged Valid .v);
      mark <= v + 42;
      $display("late fired");
   endrule
endmodule
