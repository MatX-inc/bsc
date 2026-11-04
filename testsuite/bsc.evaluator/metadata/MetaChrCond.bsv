// The chr in the condition.  A one-bit enumeration is unpacked by a
// function the evaluator unfolds (so the ord above it is not cancelled
// by ISimplify against an adjacent chr) and compared, as the condition
// of a conditional of two unpacks, whose chrs are merged into one above
// the conditional.  The condition's chr is handled at the field's level
// outside the arms, over a let-bound read that survives (written to a
// register); the merged chr is the root of the field's value, and its
// operand is the conditional, no record's: the condition's bits keep
// their own facts alone (the chr's event on the cell it read) and
// read no path.  Two ways the bits reach the chr: through a select
// of a concatenation, and through a conditional of two equal arms
// (which the evaluator takes without evaluating the condition).
typedef enum { Off, On } Sw deriving (Bits, Eq);

typedef struct {
   Bit#(4) lo;
   Sw      st;
} Knob deriving (Bits, Eq);

interface ChrCondIfc;
   method Bit#(10) out();
endinterface

function Sw viaSelect(Bit#(1) x);
   Bit#(2) w = {1'b0, x};
   Bit#(1) z = w[0:0];
   return unpack(z);
endfunction

function Sw viaEqualArms(Bit#(1) x);
   Bit#(1) y = x ^ 1;
   return (y == 0) ? unpack(x) : unpack(x);
endfunction

(* synthesize *)
module sysMetaChrCond(ChrCondIfc);
   Reg#(Bit#(1)) xa <- mkReg(0);
   Reg#(Bit#(1)) xb <- mkReg(0);
   Reg#(Bit#(1)) xd <- mkReg(0);
   Reg#(Bit#(1)) xe <- mkReg(0);
   Reg#(Bit#(4)) lo <- mkReg(0);
   Reg#(Knob) k1 <- mkReg(unpack(0));
   Reg#(Knob) k2 <- mkReg(unpack(0));
   Reg#(Bit#(1)) ta <- mkReg(0);
   Reg#(Bit#(1)) tb <- mkReg(0);
   Reg#(Bit#(1)) td <- mkReg(0);
   Reg#(Bit#(1)) te <- mkReg(0);

   // the condition's chr over a select of a concatenation
   rule selected;
      Bit#(1) a = xa;
      Bit#(1) b = xb;
      Bit#(1) d = xd;
      Knob k = Knob { lo: lo, st: (pack(viaSelect(d)) == 1) ? unpack(a) : unpack(b) };
      k1 <= k;
      ta <= a;
      tb <= b;
      td <= d;
   endrule

   // the condition's chr under a conditional of two equal arms
   rule equalArms;
      Bit#(1) a = xa;
      Bit#(1) b = xb;
      Bit#(1) e = xe;
      Knob k = Knob { lo: lo, st: (pack(viaEqualArms(e)) == 1) ? unpack(a) : unpack(b) };
      k2 <= k;
      ta <= a;
      tb <= b;
      te <= e;
   endrule

   method Bit#(10) out();
      return {pack(k1), pack(k2)};
   endmethod
endmodule
