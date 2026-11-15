import Vector::*;

import "BDPI" function ActionValue#(Bit#(32)) my_count (Bit#(8) step);

(* synthesize *)
module mkBDPIActionValueCond ();
   Reg#(UInt#(3)) s <- mkReg(0);
   Reg#(Bool) c <- mkReg(True);
   Reg#(UInt#(1)) idx <- mkReg(1);

   function ActionValue#(Tuple2#(a, a)) twice(ActionValue#(a) av);
      actionvalue
         let x <- av;
         let y <- av;
         return tuple2(x, y);
      endactionvalue
   endfunction

   // each execution of a let-bound ActionValue chosen by a dynamic
   // condition calls the function in the chosen arm
   rule r_cond (s == 0);
      ActionValue#(Bit#(32)) av = c ? my_count(1) : my_count(10);
      match {.a, .b} <- twice(av);
      $display("cond %0d %0d", a, b);
      s <= 1;
   endrule

   // the same for an ActionValue selected by a dynamic index
   rule r_sel (s == 1);
      Vector#(2, ActionValue#(Bit#(32))) avs = cons(my_count(1), cons(my_count(10), nil));
      match {.a, .b} <- twice(avs[idx]);
      $display("sel %0d %0d", a, b);
      s <= 2;
   endrule

   // and for different ActionValues that run the same call, in the two
   // arms (the rule runs once with each)
   function Bit#(32) add100(Bit#(32) x) = x + 100;
   rule r_same (s == 2 || s == 3);
      ActionValue#(Bit#(32)) one = my_count(1);
      ActionValue#(Bit#(32)) hundred = fmap(add100, one);
      match {.a, .b} <- twice(c ? one : hundred);
      $display("same %0d %0d", a, b);
      c <= !c;
      s <= s + 1;
   endrule

   rule r_done (s == 4);
      $finish(0);
   endrule
endmodule
