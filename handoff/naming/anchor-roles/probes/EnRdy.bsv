package EnRdy;
import SubIfc::*;
// Parent of two mkSub instances (s feeds, t drains, so the rules do not conflict).
//  feed : fires whenever s.put is ready, so
//         s$EN_put == WILL_FIRE_RL_feed == CAN_FIRE_RL_feed == s$RDY_put,
//         s$put_v == x (register read)
//  drain: x$EN == WILL_FIRE_RL_drain == CAN_FIRE_RL_drain == t$EN_take
//         == t$RDY_take (t$RDY_get is constant True inside mkSub)
(* synthesize *)
module sysEnRdy(Empty);
   SubIfc s <- mkSub;
   SubIfc t <- mkSub;
   Reg#(Bit#(8)) x <- mkReg(0);
   rule feed;
      s.put(x);
   endrule
   rule drain;
      let v <- t.take;
      x <= v + t.get;
   endrule
endmodule
endpackage
