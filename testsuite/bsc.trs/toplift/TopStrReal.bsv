// Top-level String and Real parameters, bound from the command line.
//
// TopParam.bsv covers the Bits case (and the wide-limb fold).  These
// two are the ones the WIDTH cannot describe: bsc exports a String at
// width 0, exactly like a Bit#(0), and a Real at width 64, exactly
// like a Bit#(64).  A binder holding only the .bir therefore cannot
// tell how to read the text after `=', which is why `Port::vtype'
// records what a port carries and why this design exists to prove it
// round-trips.
//
// Both were refused outright before -- "has width 0 (zero-width or
// non-Bit); it cannot be bound" for the String, and a Real would have
// been parsed as an integer -- so nothing reached the body at all.
//
// No reference Bluesim executable exists by design: classic Bluesim
// refuses every top-level parameter (G0099), which the sweep gates on
// via sysTopStrReal.trsonly.expected.
(* synthesize *)
module sysTopStrReal#(parameter String nm, parameter Real r, Bit#(8) k)(Empty);
   Reg#(Bit#(8)) n <- mkReg(0);

   rule step;
      n <= n + 1;
      // the String through a task argument, the Real as a double, and
      // a Bit alongside so a regression that broke only one shows up
      // as a difference rather than as an empty run
      $display("[%0d] nm=%s r=%f k=%0d", n, nm, r, k);
      if (n == 3) $finish(0);
   endrule
endmodule
