// A rule with no condition, so the register's enable resolves to the
// constant 1 and the guard it feeds can never be false.
(* synthesize *)
module mkConstEnable (Empty);
   Reg#(Bit#(8)) c <- mkReg(0);

   rule always_fires;
      c <= c + 1;
   endrule
endmodule
