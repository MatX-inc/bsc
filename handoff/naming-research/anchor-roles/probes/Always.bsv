package Always;
// always_ready / always_enabled methods; clock and reset outputs.
//  r$EN == 1 (always_enabled tick), r$D_IN == tick_d, val == r,
//  CLK_clk_out == CLK, RST_N_rst_out == RST_N
interface AIfc;
   method Bit#(8) val;
   method Action tick(Bit#(8) d);
   method Bit#(8) val2;
   interface Clock clk_out;
   interface Reset rst_out;
endinterface

(* synthesize, always_ready, always_enabled = "tick" *)
module sysAlways(AIfc);
   Reg#(Bit#(8)) r <- mkReg(0);
   Clock c  <- exposeCurrentClock;
   Reset rr <- exposeCurrentReset;
   method val = r;
   method Action tick(Bit#(8) d);
      r <= d;
   endmethod
   method val2 = r;
   interface clk_out = c;
   interface rst_out = rr;
endmodule

// Parent: uses the child's clock and reset outputs for a register.
(* synthesize *)
module sysAlwaysParent(Empty);
   AIfc a <- sysAlways;
   Reg#(Bit#(8)) q <- mkReg(0, clocked_by a.clk_out, reset_by a.rst_out);
   Reg#(Bit#(8)) p <- mkReg(0);
   rule t;
      a.tick(p);
      p <= a.val;
   endrule
   rule u;
      q <= q + 1;
   endrule
endmodule
endpackage
