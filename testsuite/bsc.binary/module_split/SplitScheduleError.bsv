package SplitScheduleError;

// This error occurs in the scheduler, after elaboration has succeeded.
(* synthesize *)
module sysSplitScheduleError(Empty);
   Reg#(Bit#(8)) value <- mkRegU;

   rule conflicting_writes;
      value <= 0;
      value <= 1;
   endrule
endmodule

endpackage
