// A small deterministic design for the kernel-context tests: the
// host_kernelctx harness initializes it in a caller-provided context
// buffer, runs it to $finish, tears the context down in place, and
// then re-initializes a fresh instance in the same buffer, expecting
// identical output both times.

(* synthesize *)
module sysKernelCtx();

   Reg#(UInt#(32))  count <- mkReg(0);
   Reg#(Bit#(128))  acc   <- mkReg(128'h1);

   rule step;
      count <= count + 1;
      acc <= (acc << 1) ^ 128'h9e3779b97f4a7c15_f39cc0605cedc834;
   endrule

   rule report (count == 25);
      $display("count=%0d", count);
      $display("acc=%h", acc);
   endrule

   rule fin (count == 50);
      $finish(0);
   endrule

endmodule
