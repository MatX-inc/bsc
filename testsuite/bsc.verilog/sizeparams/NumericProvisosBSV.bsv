package NumericProvisosBSV;

typedef struct {
   UInt#(TLog#(w))    idxBits;
   Bit#(TDiv#(w, k))  laneMask;
} FrameHdr#(numeric type w, numeric type k) deriving (Bits, Eq);

interface PacketFramer#(numeric type payload_w, numeric type lane_w);
   method Action load(Bit#(payload_w) payload, UInt#(lane_w) lane);
   method FrameHdr#(payload_w, lane_w) header();
   method UInt#(TMax#(payload_w, lane_w)) widerVal();
   method Bit#(TExp#(TMin#(payload_w, lane_w))) oneHotMin();
   method Bit#(TSub#(TMul#(payload_w, 2), lane_w)) stretchBits();
endinterface

(* synthesize *)
module mkPacketFramer (PacketFramer#(payload_w, lane_w))
   provisos (Add#(1, _pw, payload_w),
             Add#(1, _lw, lane_w),
             Add#(lane_w, _diff, payload_w),
             Add#(_t1, TLog#(payload_w), payload_w),
             Add#(_t2, TSub#(TMul#(payload_w, 2), lane_w), TAdd#(payload_w, payload_w)),
             Add#(1, _hdr, TAdd#(TLog#(payload_w), TDiv#(payload_w, lane_w))),
             Add#(1, _mx, TMax#(payload_w, lane_w)),
             Add#(1, _exp, TExp#(TMin#(payload_w, lane_w))),
             Add#(1, _str, TSub#(TMul#(payload_w, 2), lane_w)),
             Mul#(payload_w, 2, _dbl),
             Div#(payload_w, lane_w, _lanes),
             Log#(payload_w, _lg),
             Max#(payload_w, lane_w, _maxv),
             Min#(payload_w, lane_w, _minv),
             NumEq#(payload_w, payload_w));

   Reg#(FrameHdr#(payload_w, lane_w)) rHdr <- mkReg(FrameHdr {
      idxBits: fromInteger(valueOf(TLog#(payload_w))),
      laneMask: fromInteger(valueOf(TDiv#(payload_w, lane_w)))
   });
   Reg#(UInt#(TMax#(payload_w, lane_w))) rWider <- mkReg(fromInteger(valueOf(TMax#(payload_w, lane_w))));
   Reg#(Bit#(TExp#(TMin#(payload_w, lane_w)))) rOneHot <- mkReg(1);
   Reg#(Bit#(TSub#(TMul#(payload_w, 2), lane_w))) rStretch <- mkReg(fromInteger(valueOf(TSub#(TMul#(payload_w, 2), lane_w))));

   method Action load(Bit#(payload_w) payload, UInt#(lane_w) lane);
      Bit#(TMin#(payload_w, lane_w)) shamt = truncate(pack(lane));
      rHdr <= FrameHdr {
         idxBits: fromInteger(valueOf(TLog#(payload_w))) + unpack(truncate(payload)),
         laneMask: (1 << fromInteger(valueOf(TDiv#(payload_w, lane_w)))) - 1
      };
      rWider <= unpack(zeroExtend(payload)) + unpack(zeroExtend(pack(lane)));
      rOneHot <= (1 << shamt);
      rStretch <= truncate({ payload, payload });
   endmethod

   method FrameHdr#(payload_w, lane_w) header() = rHdr;
   method UInt#(TMax#(payload_w, lane_w)) widerVal() = rWider;
   method Bit#(TExp#(TMin#(payload_w, lane_w))) oneHotMin() = rOneHot;
   method Bit#(TSub#(TMul#(payload_w, 2), lane_w)) stretchBits() = rStretch;
endmodule

(* synthesize *)
module sysNumericProvisosBSV (Empty);
   PacketFramer#(12, 3) f1 <- mkPacketFramer;
   PacketFramer#(13, 4) f2 <- mkPacketFramer;
   Reg#(Bit#(4)) step <- mkReg(0);

   rule r0 (step == 0);
      $display("init f1 idx=%0d mask=%b wider=%0d oh=%h stretch=%0d",
               f1.header.idxBits, f1.header.laneMask, f1.widerVal, f1.oneHotMin, f1.stretchBits);
      $display("init f2 idx=%0d mask=%b wider=%0d oh=%h stretch=%0d",
               f2.header.idxBits, f2.header.laneMask, f2.widerVal, f2.oneHotMin, f2.stretchBits);
      f1.load(12'h3a5, 3'd5);
      f2.load(13'h12b6, 4'd9);
      step <= 1;
   endrule

   rule r1 (step == 1);
      $display("load f1 idx=%0d mask=%b wider=%h oh=%h stretch=%h",
               f1.header.idxBits, f1.header.laneMask, f1.widerVal, f1.oneHotMin, f1.stretchBits);
      $display("load f2 idx=%0d mask=%b wider=%h oh=%h stretch=%h",
               f2.header.idxBits, f2.header.laneMask, f2.widerVal, f2.oneHotMin, f2.stretchBits);
      $finish(0);
   endrule
endmodule

endpackage
