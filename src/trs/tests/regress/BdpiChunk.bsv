// repeated BDPI import under CHUNKED AOT (TRS_AOT_ONE_MODULE=0): the
// same import called from eight distinct rules puts call sites in
// more than one emitted LLVM module; each module used to define a
// strong external trs_bdpiname_c_mix diagnostic global and the
// cc -shared link died on the duplicate definition (the fix gives
// the string globals private linkage).  Byte parity vs Bluesim also
// proves the chunked artifact runs compiled.

import "BDPI" function Bit#(32) c_mix(Bit#(32) a, Bit#(32) b);

(* synthesize *)
module sysBdpiChunk(Empty);
   Reg#(Bit#(16)) cyc <- mkReg(0);
   Reg#(Bit#(32)) a0 <- mkReg(1);
   Reg#(Bit#(32)) a1 <- mkReg(2);
   Reg#(Bit#(32)) a2 <- mkReg(3);
   Reg#(Bit#(32)) a3 <- mkReg(4);
   Reg#(Bit#(32)) a4 <- mkReg(5);
   Reg#(Bit#(32)) a5 <- mkReg(6);
   Reg#(Bit#(32)) a6 <- mkReg(7);
   Reg#(Bit#(32)) a7 <- mkReg(8);

   rule step;
      cyc <= cyc + 1;
      if (cyc == 40) begin
         $display("%h %h %h %h %h %h %h %h", a0, a1, a2, a3, a4, a5, a6, a7);
         $finish(0);
      end
   endrule

   rule r0; a0 <= c_mix(a0, 32'd1); endrule
   rule r1; a1 <= c_mix(a1, 32'd2); endrule
   rule r2; a2 <= c_mix(a2, 32'd3); endrule
   rule r3; a3 <= c_mix(a3, 32'd4); endrule
   rule r4; a4 <= c_mix(a4, 32'd5); endrule
   rule r5; a5 <= c_mix(a5, 32'd6); endrule
   rule r6; a6 <= c_mix(a6, 32'd7); endrule
   rule r7; a7 <= c_mix(a7, 32'd8); endrule
endmodule
