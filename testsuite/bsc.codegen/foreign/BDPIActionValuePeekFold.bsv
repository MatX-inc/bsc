import GetPut::*;
import BDPIActionValueImport::*;

(* synthesize *)
module mkBDPIActionValuePeekFold ();
   Reg#(Bit#(32)) r <- mkReg(0);
   Get#(Bit#(32)) g = toGet(my_count(1));

   // the peeked result takes the path of a dynamic value: the evaluator
   // folds each of these to a constant whatever the result is (as it does
   // for a register), so the result is not used
   rule go;
      let p = peekGet(g);
      Bit#(64) w = zeroExtend(p);
      r <= ((p * 0) + 1) + ((p & 0) | 1) + w[63:32];
   endrule
endmodule
