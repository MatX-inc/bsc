// Methods the module exports as conflict-free, so that a parent may call
// them in either order: verifyschedule checks the orders that run the later
// of each pair first too (CF.orders), as well as the schedule's.
//   put0 and put1 both write the register last, under disjoint guards, so
//     they commute only because they never both run (a fact bv_decide
//     proves from the parent's obligations: each runs only when ready);
//   poke, peek and count touch fields no other method sets (y, and the
//     outputs), so they commute with every method by the check of the
//     fields each step reads and sets.
// Rule tally advances turn and counts cycles, after the methods.
// mutate.py's order mutant lets put0 and put1 run together and gives the
// hardware's mux of last's argument to put1, the later of the two in the
// schedule's order: the schedule's order (put1's write wins) still agrees
// with that hardware, the order that runs put1 first does not, and the
// proof fails.
interface CfOrders;
   method Action put0(Bit#(8) v);
   method Action put1(Bit#(8) v);
   method Action poke(Bit#(8) v);
   method Bit#(8) peek;
   method Bit#(8) count;
endinterface

(* synthesize *)
module mkCfOrders(CfOrders);
   Reg#(Bit#(2)) turn <- mkReg(0);
   Reg#(Bit#(8)) last <- mkReg(0);
   Reg#(Bit#(8)) y <- mkReg(0);
   Reg#(Bit#(8)) n <- mkReg(0);

   rule tally;
      n <= n + 1;
      turn <= turn + 1;
   endrule

   method Action put0(Bit#(8) v) if (turn == 0);
      last <= v;
   endmethod
   method Action put1(Bit#(8) v) if (turn == 1);
      last <= v + 1;
   endmethod
   method Action poke(Bit#(8) v);
      y <= v;
   endmethod
   method Bit#(8) peek = y;
   method Bit#(8) count = n;
endmodule
