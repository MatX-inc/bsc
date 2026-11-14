// Integer accumulators that only primSeq forces: one in the loop condition,
// one through the primSeq in ActionValue bind.  The $display reads both
// after their loops, which must not need stack in proportion to the
// iteration count.

function ActionValue#(Integer) next(Integer acc);
   actionvalue
      return acc + 2;
   endactionvalue
endfunction

(* synthesize *)
module sysPrimSeqLoop();

   rule test;
      Integer acc1 = 0;
      for (Integer i = 0; primSeq(acc1, i < 32768); i = i + 1)
         acc1 = acc1 + 2;
      Integer acc2 = 0;
      for (Integer i = 0; i < 32768; i = i + 1)
         acc2 <- next(acc2);
      $display("%0d %0d", acc1, acc2);
   endrule

endmodule
