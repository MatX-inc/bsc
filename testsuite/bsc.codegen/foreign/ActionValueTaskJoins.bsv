(* synthesize *)
module sysActionValueTaskJoins ();
   Reg#(Bool) c <- mkReg(False);
   Reg#(File) fh <- mkReg(InvalidFile);
   Reg#(Bool) done <- mkReg(False);
   Reg#(Bool) open <- mkReg(False);

   // the result of $test$plusargs, read under a condition, chooses an
   // action
   rule verbose;
      Bool v = False;
      if (c) v <- $test$plusargs("verbose");
      if (v) $display("verbose");
   endrule

   // the end of a file, read under a condition
   rule rd (!done);
      Bool eof = False;
      if (open) begin
         int ch <- $fgetc(fh);
         eof = ch == -1;
      end
      if (eof) done <= True;
   endrule
endmodule
