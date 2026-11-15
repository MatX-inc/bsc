import BDPIActionValueImport::*;

(* synthesize *)
module mkBDPIActionValueConvertTwice ();
   Reg#(Bit#(32)) r <- mkReg(0);

   // toActionValue_ makes the call where it converts the action, so the
   // two executions in the rule, which run in the same cycle, share the
   // call: the two results are one value, and their use is reported
   let av_ = toActionValue_(my_count(1));

   rule go;
      let x <- fromActionValue_(av_);
      let y <- fromActionValue_(av_);
      r <= x + y;
   endrule
endmodule
