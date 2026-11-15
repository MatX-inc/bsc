import BDPIActionValueImport::*;

typedef enum { EA, EB, EC } E deriving (Bits, Eq);

(* synthesize *)
module mkBDPIActionValueSplitIfDeadArm ();
   Reg#(Bool) c <- mkReg(False);
   Reg#(Bool) d <- mkReg(False);
   Reg#(Bit#(32)) y <- mkReg(0);

   // as BDPIActionValuePrunedArm, for an action that splitIf makes
   E v = c ? EA : EB;
   Action p = splitIf(d, action
                 Bit#(32) t = 0;
                 case (v)
                    EA: begin let u <- my_count(1); t = u; end
                    EB: t = 7;
                    EC: begin let u <- my_count(3); t = u; end
                 endcase
                 y <= t;
              endaction, noAction);

   rule q;
      p;
      c <= !c;
   endrule
endmodule
