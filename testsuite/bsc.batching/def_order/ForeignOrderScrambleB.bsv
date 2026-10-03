package ForeignOrderScrambleB;

// Compiled before ForeignOrder in the batch: the same shape, so its
// second hidden def is b__h51, interned here first.
(* synthesize *)
module mkForeignOrderScrambleB(Empty);
   rule r;
      let t <- $time;
      let x = t + 1;
      let b = t + 2;
      $display("%d", x);
      $display("%d", b);
   endrule
endmodule

endpackage
