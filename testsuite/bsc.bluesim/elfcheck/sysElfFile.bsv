// Freestanding-ELF probe: file system tasks ($fopen/$fwrite/$fgetc/
// $fclose), which exercise the host-ops file plumbing.  See
// elfcheck.exp.

(* synthesize *)
module sysElfFile();
  Reg#(Bit#(8)) cnt <- mkReg(0);
  Reg#(File)    fh  <- mkReg(InvalidFile);

  rule open (cnt == 0);
    let f <- $fopen("sysElfFile.tmp", "w");
    fh  <= f;
    cnt <= 1;
  endrule

  rule wr (cnt >= 1 && cnt < 4);
    $fwrite(fh, "line %0d\n", cnt);
    cnt <= cnt + 1;
  endrule

  rule closew (cnt == 4);
    $fclose(fh);
    cnt <= 5;
  endrule

  rule reopen (cnt == 5);
    let f <- $fopen("sysElfFile.tmp", "r");
    fh  <= f;
    cnt <= 6;
  endrule

  rule getc (cnt >= 6 && cnt < 12);
    let c <- $fgetc(fh);
    $display("c=%0d", c);
    cnt <= cnt + 1;
  endrule

  rule fin (cnt == 12);
    $fclose(fh);
    $finish(0);
  endrule
endmodule
