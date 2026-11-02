// The argument of an action-value method call is an observed value: rule
// consume calls p.take(c[1]), and c[1] (port 1 of a CReg) sees the same
// cycle's write of port 0 by rule incr.  The congruence of take's result
// must compare consume's own view of c[1] with the hardware's argument wire,
// not the hardware's c[1] with itself.
interface Pump;
   method ActionValue#(Bit#(8)) take(Bit#(8) bias);
endinterface

(* synthesize *)
module mkPump2(Pump);
   Reg#(Bit#(8)) cnt <- mkReg(10);
   method ActionValue#(Bit#(8)) take(Bit#(8) bias);
      cnt <= cnt + bias;
      return cnt;
   endmethod
endmodule

(* synthesize *)
module mkAvArg(Empty);
   Reg#(Bit#(8)) c[2] <- mkCReg(2, 0);
   Pump p <- mkPump2;
   Reg#(Bit#(8)) r <- mkReg(0);
   Reg#(Bool) phase <- mkReg(False);

   rule incr (phase);
      c[0] <= c[0] + 1;
   endrule

   rule consume;
      let v <- p.take(c[1]);
      r <= v;
      phase <= !phase;
   endrule
endmodule
