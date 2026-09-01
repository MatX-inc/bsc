import RegFile::*;

// The Bluesim RegFile is an eager, flat array in caller-provided
// storage (no sparse block tree any more), so the address space here
// is sized to what a host can reasonably provide (2^26 bytes) while
// still exercising memory-file gaps, address jumps and writes at the
// top of the range.

(* synthesize *)
module sysSparseRF(Empty) ;

   RegFile#(UInt#(26), UInt#(8)) rf <- mkRegFileFullLoad("mem2.dat");

   Reg#(UInt#(26)) rd_addr <- mkReg(10290000);
   Reg#(UInt#(26)) wr_addr <- mkReg(0);
   Reg#(UInt#(8)) val <- mkReg(0);

   rule incr_val;
      val <= val + 1;
   endrule

   rule region_1(wr_addr < 700);
      rf.upd(wr_addr,val);
      wr_addr <= wr_addr + 1;
   endrule

   rule jump_1(wr_addr == 700);
      wr_addr <= 10293874;
   endrule

   rule region_2(wr_addr > 700 && wr_addr < 10295000);
      rf.upd(wr_addr,val);
      wr_addr <= wr_addr + 1;
   endrule

   rule jump_2(wr_addr == 10295000);
      wr_addr <= 26'h3fff123;
   endrule

   rule region_3(pack(wr_addr)[25] != 0);
      rf.upd(wr_addr,val);
      wr_addr <= wr_addr + 1;
   endrule

   rule reader;
      $display("rf[%0d] = %h", rd_addr, rf.sub(rd_addr));
      rd_addr <= rd_addr + 1;
   endrule

   rule done (wr_addr == 26'h3ffffff);
      $finish(0);
   endrule

endmodule


