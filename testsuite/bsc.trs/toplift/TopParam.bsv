// Top-level module PARAMETERS and ARGUMENTS — refused by classic
// Bluesim (EBSimTopLevelArgOrParam, G0099), lifted for the -trs flow:
// trs binds them to constants at link/run time (+NAME=value).  The
// parameter is deliberately WIDE (96 bits, non-zero high limbs): the
// compiled fold once carried single-u64 port constants and silently
// zeroed wide values (the port_consts limb bug class), so this design
// asserts multi-limb folding through rule arithmetic and $display.
// No reference Bluesim executable exists by design; the sweep
// compares against the stored hand-derived golden (sysTopParam.trsonly.expected
// carries the bindings; acc_k = k*big + k*inc, checked by hand).
(* synthesize *)
module sysTopParam#(parameter Bit#(96) big, Bit#(8) inc)(Empty);
   Reg#(Bit#(96)) acc <- mkReg(0);
   Reg#(Bit#(8)) n <- mkReg(0);

   rule step;
      acc <= acc + zeroExtend(inc) + big;
      n <= n + 1;
      $display("[%0d] acc=%h inc=%h big=%h", n, acc, inc, big);
      if (n == 4) $finish(0);
   endrule
endmodule
