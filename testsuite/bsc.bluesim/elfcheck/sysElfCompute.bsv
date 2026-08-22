// Freestanding-ELF probe: wide data, string literals and real-number
// formatting.  The elfcheck.exp suite links this to a Bluesim shared
// object, runs it through the normal flow, and then requires
// bluesim_elf_check.py to certify the object freestanding (no
// DT_NEEDED, no unresolved dynamic symbols, relative-only
// relocations, no system-call instructions).

(* synthesize *)
module sysElfCompute();
  Reg#(Bit#(8))   cnt  <- mkReg(0);
  Reg#(Bit#(256)) wide <- mkReg(256'h1);
  Reg#(Bit#(64))  acc  <- mkReg(0);

  rule step (cnt < 8);
    real r = 2.5;
    cnt  <= cnt + 1;
    wide <= (wide << 3) ^ (wide >> 2) ^ zeroExtend(cnt);
    acc  <= acc + truncate(wide);
    $display("cnt=%0d wide=%h acc=%0d str=%s real=%f",
             cnt, wide, acc, "lit", r);
  endrule

  rule fin (cnt == 8);
    $finish(0);
  endrule
endmodule
