import BDPIActionValueImport::*;

interface CondMethods;
   method ActionValue#(Bit#(32)) cond_get();
   method ActionValue#(Bit#(32)) incr();
endinterface

(* synthesize *)
module mkBDPIActionValueCondMethods (CondMethods);
   Reg#(Bool) c <- mkReg(False);

   // as BDPIActionValueCondJoins, for the result of a method
   method ActionValue#(Bit#(32)) cond_get();
      Bit#(32) x = 0;
      if (c) begin x <- my_count(6); end
      return x;
   endmethod

   method ActionValue#(Bit#(32)) incr();
      let x <- my_count(7);
      return x + 1;
   endmethod
endmodule
