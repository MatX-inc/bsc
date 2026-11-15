// A loop in ActionValue whose iterations add no actions: the check in
// the body is always false during elaboration, so each iteration's action
// is empty.  Elaborating the loop should take constant stack and bounded
// heap, however many iterations it runs.

(* synthesize *)
module sysActionValueLoop();

   rule test;
      Integer n = 0;
      for (Integer i = 0; i < 16384; i = i + 1)
      begin
         if (n != i) $display("Fail %0d", i);
         n = n + 1;
      end
   endrule

endmodule
