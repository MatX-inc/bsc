import GetPut::*;
import BDPIActionValueImport::*;

typedef enum { EA, EB, EC } E deriving (Bits, Eq);

(* synthesize *)
module mkBDPIActionValuePrunedArm ();
   Reg#(Bool) c <- mkReg(False);
   Reg#(Bit#(32)) y <- mkReg(0);

   function ActionValue#(Bit#(32)) pick(E v);
      actionvalue
         Bit#(32) t = 0;
         case (v)
            EA: begin let u <- my_count(1); t = u; end
            EB: t = 7;
            EC: begin let u <- my_count(3); t = u; end
         endcase
         return t;
      endactionvalue
   endfunction

   // v is never EC, so the call in that arm is never made, and its result
   // is never used
   E v = c ? EA : EB;
   Get#(Bit#(32)) g = toGet(fromActionValue_(toActionValue_(pick(v))));

   rule step;
      let x <- g.get;
      y <= x;
      c <= !c;
   endrule
endmodule
