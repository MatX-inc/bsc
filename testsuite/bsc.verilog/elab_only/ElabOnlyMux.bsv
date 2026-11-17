interface EMuxLut;
   method Bit#(8) lookup(Bit#(8) x);
endinterface

(* synthesize *)
module mkEMuxLut(EMuxLut);
   Reg#(Bit#(8)) mask <- mkReg(8'h5a);
   method Bit#(8) lookup(Bit#(8) x);
      return x ^ mask;
   endmethod
endmodule

interface EMuxSink;
   method Action put(Bit#(8) x);
   method Bit#(8) peek();
endinterface

// Synthesized action method defaults to put C put, so non-disjoint callers
// become exclusive via conflict arbitration (excl_set -> parallel mux).
(* synthesize *)
module mkESink(EMuxSink);
   Reg#(Bit#(8)) s <- mkReg(0);
   method Action put(Bit#(8) x);
      s <= x;
   endmethod
   method Bit#(8) peek() = s;
endmodule

interface EMuxTop;
   method Action driveA(Bit#(8) x);
   method Action driveB(Bit#(8) y);
   method Bit#(8) result();
endinterface

// Exercises multi-caller argument muxing from a .ba file across:
//   * lut.lookup: disjoint callers sharing a 1-port value method (disj_set -> parallel mux)
//   * sink.put: non-disjoint conflicting callers (excl_set -> parallel mux)
//   * cr._write: non-disjoint SBR callers on Reg (not exclusive -> priority mux)
(* synthesize *)
module mkEMuxTop(EMuxTop);
   Reg#(Bit#(8)) cr <- mkReg(0);
   Reg#(Bit#(8)) lv <- mkReg(0);
   EMuxLut lut <- mkEMuxLut;
   EMuxSink sink <- mkESink;

   Wire#(Bit#(8)) reqA <- mkDWire(0);
   Wire#(Bit#(8)) reqB <- mkDWire(0);
   Reg#(Bool) enA <- mkReg(False);
   Reg#(Bool) enB <- mkReg(False);

   rule lut_from_a (enA && !enB);
      lv <= lut.lookup(reqA);
   endrule

   rule lut_from_b (enB);
      lv <= lut.lookup(reqB);
   endrule

   rule sink_from_a (enA);
      sink.put(reqA + 3);
   endrule

   rule sink_from_b (enB);
      sink.put(reqB + 4);
   endrule

   rule cr_from_a (enA);
      cr <= reqA + 1;
   endrule

   rule cr_from_b (enB);
      cr <= reqB + 2;
   endrule

   method Action driveA(Bit#(8) x);
      reqA <= x;
      enA <= (x != 0);
   endmethod

   method Action driveB(Bit#(8) y);
      reqB <= y;
      enB <= (y != 0);
   endmethod

   method Bit#(8) result();
      return sink.peek() ^ cr ^ lv;
   endmethod
endmodule
