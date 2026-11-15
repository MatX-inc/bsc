import GetPut::*;
import BDPIActionValueImport::*;

(* synthesize *)
module mkBDPIActionValuePeekSplitIf ();
   Reg#(Bit#(32)) r <- mkReg(0);
   Reg#(Bit#(32)) r2 <- mkReg(0);
   Reg#(Bool) c <- mkReg(True);

   Action a1 = action let t <- my_count(1); r2 <= t; endaction;
   Action a2 = action let t <- my_count(2); r2 <= t; endaction;
   Action a3 = action let t <- my_count(3); r2 <= t; endaction;
   Action a4 = action let t <- my_count(4); r2 <= t; endaction;

   // splitIf and the like run the chosen action where they convert it
   // (with toPrimAction), so each call is made there, and its action is
   // then dropped by the peek; the value does not use the results
   Get#(Bit#(32)) g = toGet(actionvalue
                               splitIf(c, a1, noAction);
                               nosplitIf(c, a2, noAction);
                               splitDeepIf(c, a3, noAction);
                               nosplitDeepIf(c, a4, noAction);
                               return 42;
                            endactionvalue);

   rule go;
      r <= peekGet(g);
   endrule
endmodule
