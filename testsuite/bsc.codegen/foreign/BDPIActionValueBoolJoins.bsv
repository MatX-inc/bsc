import BDPIActionValueImport::*;

(* synthesize *)
module mkBDPIActionValueBoolJoins ();
   Reg#(Bool) c <- mkReg(False);
   Reg#(Bool) rb1 <- mkReg(False);
   Reg#(Bool) rb2 <- mkReg(False);
   Reg#(Bool) rb3 <- mkReg(False);
   Reg#(Bool) rb4 <- mkReg(False);
   Reg#(Bool) rb5 <- mkReg(False);
   Reg#(Bit#(1)) rbit <- mkReg(0);
   Reg#(Bit#(32)) r <- mkReg(0);

   // A Bool result bound under a condition and used after it: the
   // compiler writes the join with the default as c && b (default False)
   // or !c || b (default True), which is still a choice by c
   rule r_false;
      Bool b = False;
      if (c) b <- my_flag(1);
      rb1 <= b;
   endrule

   rule r_true;
      Bool b = True;
      if (c) b <- my_flag(2);
      rb2 <= b;
   endrule

   rule r_else;
      Bool b;
      if (c) b <- my_flag(3);
      else b = False;
      rb3 <= b;
   endrule

   rule r_not;
      Bool b = False;
      if (!c) b <- my_flag(4);
      rb4 <= b;
   endrule

   rule r_and;
      Bool b = False;
      if (c) b <- my_flag(5);
      rb5 <= b && c;
   endrule

   // one bit of a result
   rule r_bit;
      Bit#(1) b = 0;
      if (c) begin
         let y <- my_count(6);
         b = y[0];
      end
      rbit <= b;
   endrule

   // a Bool result that chooses a value
   rule r_choose;
      Bit#(32) x = 0;
      Bool b = False;
      if (c) b <- my_flag(7);
      if (b) x = 3;
      r <= x;
   endrule
endmodule
