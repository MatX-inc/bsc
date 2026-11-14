package TwoRules;
// ra and rb have identical guards (go); both always win (no conflict), so
// CAN_FIRE_RL_ra == CAN_FIRE_RL_rb == WILL_FIRE_RL_ra == WILL_FIRE_RL_rb == go
// == a$EN == b$EN.  tick has CAN_FIRE constant True.  rc conflicts with ra
// (both write a) so WILL_FIRE_RL_rc != CAN_FIRE_RL_rc.
(* synthesize *)
module sysTwoRules(Empty);
   Reg#(Bool) go <- mkReg(False);
   Reg#(Bool) go2 <- mkReg(False);
   Reg#(Bit#(8)) a <- mkReg(0);
   Reg#(Bit#(8)) b <- mkReg(0);
   rule ra (go);
      a <= a + 1;
   endrule
   rule rb (go);
      b <= b + 1;
   endrule
   (* descending_urgency = "ra, rc" *)
   rule rc (go2);
      a <= 0;
   endrule
   rule tick;
      go <= !go;
      go2 <= !go2;
   endrule
endmodule
endpackage
