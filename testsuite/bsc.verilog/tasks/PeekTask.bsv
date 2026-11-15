import GetPut::*;

(* synthesize *)
module sysPeekTask ();
   Reg#(Bit#(32)) r <- mkReg(42);
   Reg#(File) fh <- mkRegU;
   Reg#(Bool) opened <- mkReg(False);

   // peekGet reads the value of g.get without running it.  The action
   // displays a message, closes a file (a call with no result) and reads
   // the time, whose result the value does not use: nothing to report.
   Get#(Bit#(32)) g = toGet(actionvalue
                               $display("get");
                               $fclose(fh);
                               let t <- $time;
                               return r;
                            endactionvalue);

   rule open (!opened);
      let f <- $fopen("/dev/null", "w");
      fh <= f;
      opened <= True;
   endrule

   rule go (opened);
      let p = peekGet(g);
      let x <- g.get;
      $display("peek %0d get %0d", p, x);
      $finish(0);
   endrule
endmodule
