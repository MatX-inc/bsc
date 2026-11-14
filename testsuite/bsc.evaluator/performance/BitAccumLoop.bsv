// A Bit accumulator compared on every iteration of a loop that adds no
// actions.  The $display reads it after the loop, which must not need
// stack in proportion to the iteration count.

(* synthesize *)
module sysBitAccumLoop();

   rule test;
      Bit#(32) acc = 0;
      for (Integer i = 0; i < 16384; i = i + 1)
      begin
         if (acc == 1) $display("odd");
         acc = acc + 2;
      end
      $display("%0d", acc);
   endrule

endmodule
