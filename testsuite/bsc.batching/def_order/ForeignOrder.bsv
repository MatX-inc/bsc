package ForeignOrder;

// a and b depend on the value of one foreign call and are used by two
// others, so both are inlined into the always block, between the calls;
// nothing orders them against each other.  Their hidden names are a__h50
// and b__h51.
(* synthesize *)
module mkForeignOrder(Empty);
   rule r;
      let t <- $time;
      let a = t + 1;
      let b = t + 2;
      $display("%d", a);
      $display("%d", b);
   endrule
endmodule

endpackage
