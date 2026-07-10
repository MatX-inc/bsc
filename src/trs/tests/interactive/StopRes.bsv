// $stop-vs-$finish: $stop pauses (resumable), $finish terminates.
(* synthesize *)
module sysStopRes();
   Reg#(UInt#(8)) cyc <- mkReg(0);
   rule tick;
      cyc <= cyc + 1;
      $display("cyc %0d", cyc);
      if (cyc == 3) $stop;
      if (cyc == 7) $finish(0);
   endrule
endmodule
