import BDPIActionValueImport::*;

typedef enum { EA, EB, EC } E deriving (Bits, Eq);

(* synthesize *)
module mkBDPIActionValueCondJoins ();
   Reg#(Bit#(32)) r <- mkReg(0);
   Reg#(Bit#(32)) r2 <- mkReg(0);
   Reg#(Bit#(32)) r3 <- mkReg(0);
   Reg#(Bool) c <- mkReg(False);
   Reg#(Bool) d <- mkReg(False);
   Reg#(E) e <- mkReg(EA);

   // a result bound under a condition is used after the condition: the
   // value is chosen by the same condition as the call
   rule cond_bind;
      Bit#(32) x = 0;
      if (c) begin x <- my_count(1); end
      r <= x;
   endrule

   // the same for an else-if chain ...
   rule else_if;
      Bit#(32) x = 7;
      if (!c) begin x <- my_count(2); end
      else if (d) begin x <- my_count(3); end
      r2 <= x;
   endrule

   // ... and for a case
   rule case_arms;
      Bit#(32) x = 7;
      case (e)
         EA: x <- my_count(4);
         EB: x <- my_count(5);
      endcase
      r3 <= x;
   endrule
endmodule
