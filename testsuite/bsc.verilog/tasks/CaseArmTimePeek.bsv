import FIFO::*;
import GetPut::*;

typedef enum { EA, EB, EC } E deriving (Bits, Eq);

// The evaluator removes the arm of the case for a value that the case
// never has, and the $time call in it; a peekGet elsewhere in the module
// drops no action of the rule
(* synthesize *)
module sysCaseArmTimePeek();
   Reg#(Bool) c <- mkReg(False);
   Reg#(Bit#(8)) y <- mkReg(0);
   FIFO#(Bit#(8)) f <- mkFIFO;

   rule show;
      E v = c ? EA : EB;
      case (v)
         EA: $display("a %t", $time);
         EB: $display("b %t", $time);
         EC: $display("c %t", $time);
      endcase
      c <= !c;
   endrule

   rule peek;
      y <= peekGet(toGet(f));
   endrule
endmodule
