import BDPIActionValueImport::*;

(* synthesize *)
module mkBDPIActionValueMFixAfterWrite ();
   Reg#(Bit#(32)) r1 <- mkReg(0);
   Reg#(Bit#(32)) s <- mkReg(0);

   // as in BDPIActionValueMFix, after a register write in the action
   function ActionValue#(Tuple2#(Bit#(32), Bit#(32))) loop(Tuple2#(Bit#(32), Bit#(32)) x);
      actionvalue
         s <= 5;
         let y <- my_count(5);
         return tuple2(y, tpl_1(x));
      endactionvalue
   endfunction

   rule r;
      let v <- mfix(loop);
      r1 <= tpl_2(v);
   endrule
endmodule
