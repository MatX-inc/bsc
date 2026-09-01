// A design whose whole lifecycle must make no allocator calls: it is
// constructed in caller-provided storage, initialized, run through
// busy cycles -- $display traffic with strings, wide values and
// reals, RegFile (preloaded) and BRAM traffic, FIFO and submodule
// activity, $swrite through a stack buffer -- and shut down, while
// the host_hostalloc harness watches every allocator.
//
// Wide division/remainder are deliberately NOT used: their runtime
// implementation still allocates internal temporaries.
//
// The 'poke' register is written by no rule: the harness writes it
// through the state buffer at its published offset (and reads 'cnt',
// 'wide' and 'mirror' back the same way) to validate the state
// write-through in both directions.

import RegFile::*;
import FIFO::*;
import BRAMCore::*;

interface HASub;
   method Action bump(Bit#(64) x);
   method Bit#(256) acc();
endinterface

(* synthesize *)
module mkHostAllocSub(HASub);
   Reg#(Bit#(256)) r <- mkReg(256'h0123456789abcdef_fedcba9876543210_0f1e2d3c4b5a6978_8796a5b4c3d2e1f0);

   method Action bump(Bit#(64) x);
      r <= {r[191:0], r[255:192] ^ x};
   endmethod

   method acc = r._read;
endmodule

(* synthesize *)
module sysHostAlloc();
   Reg#(Bit#(32))  cnt    <- mkReg(0);
   Reg#(Bit#(96))  wide   <- mkReg(96'h0123_4567_89ab_cdef_0246_8ace);
   Reg#(Bit#(8))   tiny  <- mkReg(8'h5a);
   Reg#(Bit#(32))  poke   <- mkReg(32'h11111111);   // host-written only
   Reg#(Bit#(32))  mirror <- mkReg(0);
   RegFile#(Bit#(4), Bit#(48)) rf <- mkRegFileLoad("sysHostAlloc.dat", 0, 15);
   FIFO#(Bit#(80)) fif  <- mkSizedFIFO(4);
   BRAM_PORT#(Bit#(4), Bit#(40)) bram <- mkBRAMCore1(16, False);
   HASub sub <- mkHostAllocSub;

   rule step;
      cnt <= cnt + 1;
      wide <= {wide[94:0], wide[95] ^ wide[62]};
      tiny <= tiny + 3;
      mirror <= poke;
      sub.bump(zeroExtend(cnt) ^ 64'h9e3779b97f4a7c15);
   endrule

   rule rftraffic;
      rf.upd(truncate(cnt), {cnt[15:0], cnt});
   endrule

   rule bramtraffic;
      bram.put(cnt[0] == 1, truncate(cnt >> 1), {8'hb5, cnt});
   endrule

   rule fenq;
      fif.enq({cnt[15:0], 32'h0eb0_0e51, cnt});
   endrule

   rule fdeq (cnt > 4);
      fif.deq;
   endrule

   rule report (cnt[5:0] == 6'd63);
      $display("t=%0t cnt=%0d tiny=%0d wide=%h poke=%h mirror=%h rf=%h fifo=%h bram=%h acc=%h tag=%s pi=%e half=%f",
               $time, cnt, tiny, wide, poke, mirror,
               rf.sub(7), fif.first, bram.read, sub.acc,
               (cnt[6] == 0) ? "lo" : "hi",
               3.141592653589793, 0.5);
   endrule

   rule swr (cnt[5:0] == 6'd31);
      Bit#(64) s <- $swriteAV("%0d", cnt);
      $display("swrite=%h", s);
   endrule

   rule fin (cnt == 32'd2000);
      $finish(0);
   endrule
endmodule
