// Interactive witness for $finish edge completion (compiled paths):
// `late` is forced AFTER `fin` on the same edge (RWire wset SB wget),
// so its state write lands post-$finish (mark = cyc + 42) with its
// $display suppressed; `count` is also scheduled after `fin` (its
// finish-edge display is suppressed), so cyc advances too.  The .cmd
// runs to $finish on the JIT engine and peeks both registers — a
// mid-edge-aborting engine answers mark=0/cyc=1000000.  The finish
// cycle is large so the background body compile is warm long before
// the finish edge (short runs may race compiled vs interp-fallback
// bodies; both share the contract, but the witness wants the
// compiled one).
(* synthesize *)
module sysFinishPeek();
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) mark <- mkReg(0);
   RWire#(UInt#(32)) w <- mkRWire;

   rule count;
      cyc <= cyc + 1;
   endrule

   rule fin (cyc == 1000000);
      $display("finishing at %0d mark %0d", cyc, mark);
      w.wset(cyc);
      $finish(0);
   endrule

   rule late (w.wget matches tagged Valid .v);
      mark <= v + 42;
      $display("late fired");
   endrule
endmodule
