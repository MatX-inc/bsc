// A design whose loading must perform no allocator calls, exercising
// every kind of generated literal and every runtime consumer of them:
// wide literals (register resets and rule-body constants), string
// literals (formats, %s arguments and a dynamically selected string),
// the file tasks ($fopen/$fwrite/$fgetc/$fclose), $test$plusargs, and
// a foreign (BDPI) function taking a string.
//
// The normal flow checks that the converted consumers still behave;
// the host_loadalloc harness dlopen()s the model and checks that its
// static initialization makes no allocator calls at all.

import "BDPI" function Bit#(32) loadalloc_strlen(String s);

(* synthesize *)
module sysLoadAlloc();

   Reg#(Bit#(256)) r1 <- mkReg(256'h123456789abcdef0_fedcba9876543210_0f1e2d3c4b5a6978_8796a5b4c3d2e1f0);
   Reg#(Bit#(320)) r2 <- mkReg(0);
   Reg#(UInt#(16)) count <- mkReg(0);
   Reg#(File)      fh <- mkRegU;

   rule step;
      count <= count + 1;
      r1 <= (r1 << 1) ^ 256'haaaa_bbbb_cccc_dddd;
      r2 <= r2 + 320'h1_00000000_00000001;
   endrule

   rule disp (count == 10);
      $display("r1=%h", r1);
      $display("r2=%h", r2);
      $display("pick=%s", (r1[0] == 1) ? "odd" : "even");
   endrule

   rule bdpi (count == 15);
      $display("len=%0d", loadalloc_strlen("four"));
   endrule

   rule fwrite (count == 20);
      let f <- $fopen("sysLoadAlloc.dat", "w");
      fh <= f;
   endrule

   rule fput (count == 21);
      $fwrite(fh, "%0d", 42);
      $fclose(fh);
   endrule

   rule fread (count == 22);
      let f <- $fopen("sysLoadAlloc.dat", "r");
      fh <= f;
   endrule

   rule fget (count == 23);
      let c0 <- $fgetc(fh);
      let c1 <- $fgetc(fh);
      $fclose(fh);
      $display("file=%c%c", c0, c1);
      Bool p <- $test$plusargs("loadalloc");
      $display("plusarg=%b", p);
   endrule

   rule fin (count == 30);
      $finish(0);
   endrule

endmodule
