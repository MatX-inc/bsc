package ForeignOrderScrambleA;

// Compiled after ForeignOrderScrambleB and before ForeignOrder: its first
// hidden def is a__h50, interned after b__h51.
(* synthesize *)
module mkForeignOrderScrambleA(Empty);
   rule r;
      let t <- $time;
      let a = t + 1;
      let y = t + 2;
      $display("%d", a);
      $display("%d", y);
   endrule
endmodule

endpackage
