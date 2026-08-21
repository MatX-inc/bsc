// NEGATIVE: a top module with bindable arguments AND an additional
// input clock argument.  Bindings supply constants, never waveforms,
// so both bsc's -trs link and trs itself refuse this loudly (v1);
// classic Bluesim keeps its EBSimTopLevelArgOrParam refusal (the
// Bit#(8) argument triggers it — clock arguments alone never did).
(* synthesize *)
module sysTopClkArg#(Bit#(8) k, Clock c2)(Empty);
   Reg#(Bit#(8)) n <- mkReg(0);
   rule r;
      n <= n + k;
      $display("n=%h", n);
      if (n >= 8) $finish(0);
   endrule
endmodule
