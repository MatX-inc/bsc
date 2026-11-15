
String readFile = "gettests.dat";

(* synthesize *)
module sysUngetC_LetBound () ;

   Reg#(UInt#(2)) s <- mkReg(0);
   Reg#(File) fh <- mkReg(InvalidFile);
   Reg#(int) r1 <- mkReg(0);
   Reg#(int) r2 <- mkReg(0);

   rule open (s == 0);
      let lfh <- $fopen(readFile, "r");
      fh <= lfh;
      s <= 1;
   endrule

   // each execution of the let-bound $ungetc is its own call, with its
   // own result; a $fgetc between them reads the character back, so that
   // no more than one character is pushed back at a time
   rule unget (s == 1);
      int c <- $fgetc(fh);
      ActionValue#(int) unget_c = $ungetc(truncate(pack(c)), fh);
      let x1 <- unget_c;
      int c_again <- $fgetc(fh);
      let x2 <- unget_c;
      r1 <= x1;
      r2 <= x2;
      s <= 2;
   endrule

   rule read (s == 2);
      int c1 <- $fgetc(fh);
      int c2 <- $fgetc(fh);
      int c3 <- $fgetc(fh);
      $display("read %c%c%c", c1, c2, c3);
      $display("both ungot = %b", r1 != -1 && r2 != -1);
      $fclose(fh);
      s <= 3;
   endrule

   rule finish (s == 3);
      $finish(0);
   endrule

endmodule
