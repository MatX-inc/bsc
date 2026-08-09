import BRAMCore::*;

(* synthesize *)
module sysBramVcd ();
   BRAM_PORT#(Bit#(8), Bit#(16)) ram <- mkBRAMCore1(256, False);
   Reg#(UInt#(8)) cyc <- mkReg(0);

   rule count;
      cyc <= cyc + 1;
   endrule

   rule wr (cyc % 3 == 0);
      ram.put(True, truncate(pack(cyc)), zeroExtend(pack(cyc)) + 100);
   endrule

   rule rd (cyc % 3 == 1);
      ram.put(False, truncate(pack(cyc) - 1), 0);
   endrule

   rule show (cyc % 3 == 2);
      $display("cyc %0d read %h", cyc, ram.read);
   endrule

   rule done (cyc == 20);
      $finish(0);
   endrule
endmodule
