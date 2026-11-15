import BDPIActionValueImport::*;

(* synthesize *)
module mkBDPIActionValueConvertTwiceExclusive ();
   Reg#(Bool) c <- mkReg(False);
   Reg#(Bit#(32)) r <- mkReg(0);
   Reg#(Bit#(32)) r2 <- mkReg(0);

   let av_ = toActionValue_(my_count(1));
   Action tick = fromPrimAction(toPrimAction(action
                                                let t <- my_count(2);
                                             endaction));

   // two executions of a converted action in one rule share the call, but
   // only one of these runs in a cycle, and its result is the call's
   rule go;
      if (c) begin
         let x <- fromActionValue_(av_);
         r <= x;
      end
      else begin
         let y <- fromActionValue_(av_);
         r2 <= y;
      end
   endrule

   // and nothing uses the result of these
   rule twice;
      tick;
      tick;
   endrule
endmodule
