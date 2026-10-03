package InternOrder;

// Anything to compile: the test looks at the interning, not the output.
(* synthesize *)
module mkInternOrder(Empty);
   Reg#(UInt#(8)) count <- mkReg(0);
   rule step;
      count <= count + 1;
   endrule
endmodule

endpackage
