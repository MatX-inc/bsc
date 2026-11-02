interface Odd;
   method Action  a(Bit#(8) x);
   method Action  b(Bit#(8) x);
   method Action  c;
   method Bit#(8) v;
endinterface

// Generic-child stress test: a BVI child whose SchedInfo uses P (parallel
// composable) and an SBR self-relation on an argument-less action method.
// P parses and survives into VModInfo, but ASchedule.getStateInfo reads only
// sCF / sSB / sSBR, so bsc schedules the P pair (a, c) as if it were C.
// (ME and EXT also parse but crash bsc: see negative/BviMEExtCrash.bsv.)
// Every method pair, self pairs included, is annotated exactly once.
// No Verilog for OddPrim is needed to produce the .ba.
import "BVI" OddPrim =
module mkOdd (Odd);
   default_clock clk(CLK, (*unused*) CLK_GATE);
   default_reset rst(RST_N);
   method a(A_IN) enable(EN_A);
   method b(B_IN) enable(EN_B);
   method c() enable(EN_C);
   method V_OUT v;
   schedule a P c;
   schedule v CF v;
   schedule v SB (a, b, c);
   schedule (a, b) C (a, b);
   schedule b C c;
   schedule c SBR c;
endmodule

// ra and rb have disjoint guards, so their a / b calls (a C b) never meet;
// rc calls c, which pairs with a as P (scheduled as C), so ra and rc
// conflict (rc is blocked when ra or rb fires, i.e. when r[1:0] < 2);
// rd also calls c (c SBR c), so rc and rd can fire together.
//
// Checked (bsc 2026.07-6): rc and rd blocked by ra and rb (o.a vs o.c is
// reported as a conflict); order ra, rb, rc, rd.
//
// Compile: bsc -u -verilog -elab -show-schedule BviSchedKinds.bsv
(* synthesize *)
module mkUseOdd(Empty);
   Odd o <- mkOdd;
   Reg#(Bit#(8)) r <- mkReg(0);
   Reg#(Bit#(8)) q <- mkReg(0);

   rule ra (r[1:0] == 0);
      o.a(r);
   endrule

   rule rb (r[1:0] == 1);
      o.b(r);
   endrule

   rule rc;
      o.c;
      r <= r + o.v;
   endrule

   rule rd (q != 0);
      o.c;
      q <= q - 1;
   endrule
endmodule
