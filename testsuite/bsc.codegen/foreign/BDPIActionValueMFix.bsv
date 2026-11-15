import "BDPI" function ActionValue#(Bit#(32)) my_count (Bit#(8) step);

(* synthesize *)
module mkBDPIActionValueMFix ();
   Reg#(Bit#(32)) r1 <- mkReg(0);

   // mfix gives loop the value of the ActionValue it returns, read without
   // running it: its first field is the result of my_count, which the
   // rule uses through the second field
   function ActionValue#(Tuple2#(Bit#(32), Bit#(32))) loop(Tuple2#(Bit#(32), Bit#(32)) x);
      actionvalue
         let y <- my_count(5);
         return tuple2(y, tpl_1(x));
      endactionvalue
   endfunction

   rule r;
      let v <- mfix(loop);
      r1 <= tpl_2(v);
   endrule
endmodule
