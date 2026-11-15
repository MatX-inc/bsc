import GetPut::*;
import BDPIActionValueImport::*;

(* synthesize *)
module mkBDPIActionValuePeekUndet ();
   Reg#(Bit#(32)) r1 <- mkReg(0);
   Reg#(Bit#(32)) r2 <- mkReg(0);
   Reg#(Bit#(32)) r3 <- mkReg(0);
   Reg#(Bool) c <- mkReg(True);
   Action a_undet = ?;
   ActionValue#(Bit#(32)) u = ?;

   // a call after an undetermined action in an if is not made in a peek;
   // one after an undetermined action (or ActionValue) alone is made, since
   // the world of an undetermined action is ActionWorld, but its action is
   // dropped: the rule uses each result, which exists in neither case
   Get#(Bit#(32)) g1 = toGet(actionvalue
                                a_undet;
                                let x <- my_count(1);
                                return x;
                             endactionvalue);
   Get#(Bit#(32)) g2 = toGet(actionvalue
                                if (c) a_undet;
                                let x <- my_count(2);
                                return x;
                             endactionvalue);
   Get#(Bit#(32)) g3 = toGet(actionvalue
                                let y <- u;
                                let x <- my_count(3);
                                return x + y;
                             endactionvalue);

   rule go1;
      r1 <= peekGet(g1);
   endrule
   rule go2;
      r2 <= peekGet(g2);
   endrule
   rule go3;
      r3 <= peekGet(g3);
   endrule
endmodule
