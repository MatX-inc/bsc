import FIFO::*;
import RegFile::*;
import BRAMCore::*;

// A design with a known mix of state elements, used to validate the
// non-allocating introspection descriptors the code generator emits
// (state elements with dotted instance names, kinds, widths and a
// flat byte layout, plus the top module's input and output ports).
// The accompanying host harness (host_introspect.cxx) walks the
// descriptor tables through the bk_* kernel accessors before
// bk_sync_init() and checks names, kinds, geometry and the layout
// invariants documented in bluesim_introspection.h.

interface SubIfc;
   method Action note(Bit#(9) v);
   method Bit#(9) acc();
endinterface

// A separately synthesized submodule, so its state elements appear
// under a nested dotted instance name (top.sub.*)
(* synthesize *)
module mkIntrospectSub (SubIfc);
   Reg#(Bit#(9))   total <- mkReg(0);
   FIFO#(Bit#(65)) pipe  <- mkSizedFIFO(5);  // wide FIFO, depth 5

   rule drain;
      pipe.deq();
   endrule

   method Action note(Bit#(9) v);
      total <= total + v;
      pipe.enq(zeroExtend(v));
   endmethod
   method acc = total;
endmodule

interface IntrospectIfc;
   method Action push(Bit#(17) x);
   method Bit#(17) peek();
   method ActionValue#(Bit#(5)) grab(Bit#(3) a);
endinterface

(* synthesize *)
module sysIntrospect (IntrospectIfc);
   Reg#(Bool)      flag   <- mkReg(False);       // 1-bit reg
   Reg#(Bit#(8))   byte_r <- mkReg(17);          // 8-bit reg
   Reg#(Bit#(48))  mid_r  <- mkRegU;             // 48-bit reg
   Reg#(Bit#(128)) wide_r <- mkReg(0);           // wide reg
   Reg#(Bit#(10))  cnt    <- mkReg(0);
   RegFile#(Bit#(4), Bit#(20)) rf <- mkRegFile(2, 11);  // 10 entries
   BRAM_PORT#(Bit#(6), Bit#(33)) ram <- mkBRAMCore1(64, False);
   FIFO#(Bit#(13)) q2     <- mkFIFO;             // FIFO2, depth 2
   FIFO#(Bit#(70)) qw     <- mkFIFO1;            // wide FIFO, depth 1
   RWire#(Bit#(5)) rw     <- mkRWire;
   PulseWire       pw     <- mkPulseWire;        // zero-width wire
   SubIfc          sub    <- mkIntrospectSub;    // nested state

   rule tick;
      cnt <= cnt + 1;
      flag <= !flag;
      wide_r <= wide_r + 1;
      mid_r <= zeroExtend(cnt);
      ram.put(True, truncate(cnt), zeroExtend(cnt));
      sub.note(truncate(cnt));
      if ((cnt >= 2) && (cnt <= 11))
         rf.upd(truncate(cnt), zeroExtend(cnt) + 3);
      if (cnt == 20) begin
         $display("acc = %0d", sub.acc());
         $display("rf5 = %0d", rf.sub(5));
         $finish(0);
      end
   endrule

   method Action push(Bit#(17) x);
      q2.enq(truncate(x));
   endmethod

   method Bit#(17) peek();
      return zeroExtend(q2.first());
   endmethod

   method ActionValue#(Bit#(5)) grab(Bit#(3) a);
      pw.send();
      rw.wset(zeroExtend(a));
      return zeroExtend(a) + 1;
   endmethod
endmodule
