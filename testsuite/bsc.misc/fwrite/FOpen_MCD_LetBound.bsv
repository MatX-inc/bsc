
String dumpFile = "FOpen_MCD_LetBound.dat.out";

(* synthesize *)
module sysFOpen_MCD_LetBound () ;

   Reg#(Bool) opened <- mkReg(False);
   Reg#(File) fh1 <- mkReg(InvalidFile);
   Reg#(File) fh2 <- mkReg(InvalidFile);

   // each execution of the let-bound $fopen opens its own channel
   rule open (!opened);
      ActionValue#(File) open_mcd = $fopen(dumpFile);
      let lfh1 <- open_mcd;
      let lfh2 <- open_mcd;
      fh1 <= lfh1;
      fh2 <= lfh2;
      opened <= True;
   endrule

   rule close (opened);
      $display("same channel = %b", fh1 == fh2);
      $fclose(fh1);
      $fclose(fh2);
      $finish(0);
   endrule

endmodule
