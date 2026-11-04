import FIFOF::*;

// Cells outside the gate.  Only a cell of type Bit n carries a validity
// or a view; the cells of every other type exercised here carry none:
// the Integer and String fields moved out of a struct beside its Bit
// field, the actions a rule joins, the enumeration and the struct read
// from a register and held before unpacking, the enumeration a chr of
// computed bits builds, the Bool methods of an import-BVI FIFO and the
// actionvalue method of a synthesized submodule.
typedef enum { Red, Green, Blue } Hue deriving (Bits, Eq);

typedef struct {
   Bit#(4) lo;
   Hue     hu;
} Pair deriving (Bits, Eq);

typedef struct {
   Integer n;
   String  tag;
   Bit#(8) v;
} Mixed;

interface Sub;
   method ActionValue#(Bit#(8)) take();
endinterface

(* synthesize *)
module mkSub(Sub);
   Reg#(Bit#(8)) r <- mkReg(0);

   method ActionValue#(Bit#(8)) take();
      r <= r + 1;
      return r;
   endmethod
endmodule

interface NonBitIfc;
   method Bit#(8) out();
   method Bool busy();
endinterface

(* synthesize *)
module sysMetaNonBit(NonBitIfc);
   Reg#(Bit#(8)) raw <- mkReg(0);
   Reg#(Bit#(8)) acc <- mkReg(0);
   Reg#(Hue) hue <- mkReg(Red);
   Reg#(Pair) pr <- mkReg(Pair { lo: 0, hu: Red });
   FIFOF#(Bit#(8)) q <- mkFIFOF;
   Sub sub <- mkSub;

   // a struct whose fields are moved out when it is evaluated: an
   // Integer, a String and a Bit field
   Mixed mx = Mixed { n: 3, tag: "mixed", v: raw };

   // actions joined under a rule, one of them with a string argument
   rule show;
      $display("%s %d", mx.tag, mx.v + fromInteger(mx.n));
      acc <= acc + 1;
   endrule

   // the enumeration read from its register, held before unpacking,
   // and a chr of a byte's low bits (a repack, on the cell of the bits)
   rule turn;
      Hue h = hue;
      Hue g = unpack(raw[1:0]);
      hue <= (h == Red) ? g : Red;
   endrule

   // a struct read from its register, held before unpacking, and its
   // fields moved out
   rule pair;
      Pair p = pr;
      pr <= Pair { lo: p.lo + 1, hu: p.hu };
   endrule

   // an actionvalue method of an instance whose port types are saved
   (* descending_urgency = "take, drain" *)
   rule take (q.notFull);
      let x <- sub.take();
      q.enq(x);
   endrule

   rule drain (q.notEmpty);
      raw <= q.first;
      q.deq;
   endrule

   method Bit#(8) out();
      return acc;
   endmethod

   // a Bool method of the import-BVI FIFO
   method Bool busy();
      return q.notEmpty;
   endmethod
endmodule
