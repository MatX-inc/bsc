// A 3-port CReg (mkCReg).  A write on port i is visible to reads
// on ports j > i in the same cycle (port_i SBR port_j), so the schedule must
// execute incr (port 0) -> dbl (port 1) -> obs (port 2), and all three can
// fire in one cycle.  mkCReg(3, ...) instantiates the 5-port CRegN5 primitive;
// ports 3 and 4 are unused.  Child methods appear as c.port0__read,
// c.port0__write, c.port1__read, ...
//
// Checked (bsc 2026.07-6): order incr, dbl, obs; nothing blocked.
//
// Compile: bsc -u -verilog -elab -show-schedule CRegPorts.bsv
(* synthesize *)
module mkCRegPorts(Empty);
   Reg#(Bit#(8)) c[3] <- mkCReg(3, 0);
   Reg#(Bit#(8)) seen  <- mkReg(0);
   Reg#(Bool)    phase <- mkReg(False);

   rule incr (phase);
      c[0] <= c[0] + 1;
   endrule

   // reads the value incr wrote this cycle
   rule dbl (c[1] < 100);
      c[1] <= c[1] * 2;
   endrule

   // sees both updates
   rule obs;
      seen  <= c[2];
      phase <= !phase;
   endrule
endmodule
