import BDPIActionValueImport::*;

(* synthesize *)
module mkBDPIActionValueUndetCond ();
   Reg#(Bit#(32)) r <- mkReg(0);

   // a condition that is a don't-care value is one undetermined value for
   // each place that uses it (here the call and the join of its result),
   // which later passes can choose differently, so the use of the result
   // is not under the condition of the call: the error says why
   rule go;
      Bool c = ?;
      Bit#(32) y = 5;
      if (c) begin
         let x <- my_count(2);
         y = x;
      end
      r <= y;
   endrule
endmodule
