interface Source;
   method ActionValue#(Bit#(8)) pop();
endinterface

interface Pump;
   method ActionValue#(Bit#(8)) take(Bit#(8) bias);
   method Bit#(8) total;
endinterface

// Child, separately synthesized: the parent sees pop as a child method with
// EN_pop, RDY_pop and the result port pop.  pop reads and writes cnt, so it
// is exported as (pop C pop).
(* synthesize *)
module mkSource(Source);
   Reg#(Bit#(8)) cnt <- mkReg(10);

   method ActionValue#(Bit#(8)) pop() if (cnt != 0);
      cnt <= cnt - 1;
      return cnt;
   endmethod
endmodule

// Actionvalue methods.
//  * child AV method src.pop called from a rule (drain) and from an own AV
//    method (take); the two callers conflict (pop C pop), and the method is
//    more urgent by default, so drain is blocked in cycles where EN_take is set
//  * own AV method take: RDY_take = src.RDY_pop, result = src.pop + bias
//  * total reads acc, which drain writes (total SB drain)
//
// Checked (bsc 2026.07-6): mkSource exports pop C pop; mkPump order
// take, total, drain, with drain blocked by take.
//
// Compile: bsc -u -verilog -elab -show-schedule ActionValuePump.bsv
//          (writes mkSource.ba and mkPump.ba)
(* synthesize *)
module mkPump(Pump);
   Source src <- mkSource;
   Reg#(Bit#(8)) acc <- mkReg(0);
   Reg#(Bool)    en  <- mkReg(True);

   rule drain (en);
      let v <- src.pop;
      acc <= acc + v;
   endrule

   method ActionValue#(Bit#(8)) take(Bit#(8) bias);
      let v <- src.pop;
      return v + bias;
   endmethod

   method Bit#(8) total = acc;
endmodule
