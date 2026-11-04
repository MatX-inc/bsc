// The bits under a chr of a conditional.  A one-bit enumeration is
// unpacked from a conditional of two reads (the coercion of one bit
// is not held: the chr is applied to the conditional and pushed into
// its arms), and written from a conditional of two unpacks (a chr in
// each arm, merged into one above the conditional): either way the
// field's value is chr (if sel a b), whose bits are no cell, so the
// field's path reaches neither arm's cell, and each arm's read keeps
// its own facts alone (the chr it was converted by, as an event).
// Beside it a two-bit enumeration from the same two shapes of
// conditional, under a condition of its own: that coercion is held,
// the conditional's bits are the cell the held unpack reads, and that
// cell reads the field's path and its declared type.  The arms are
// also written to registers of their own, so that their cells survive
// to the rebuild.
typedef enum { Off, On } Sw deriving (Bits, Eq);
typedef enum { N, E, S, W } Quad deriving (Bits, Eq);

typedef struct {
   Bit#(4) lo;
   Sw      st;
   Quad    q;
} Knob deriving (Bits, Eq);

interface ChrArmIfc;
   method Bit#(7) out();
endinterface

(* synthesize *)
module sysMetaChrArm(ChrArmIfc);
   Reg#(Bit#(1)) xa <- mkReg(0);
   Reg#(Bit#(1)) xb <- mkReg(0);
   Reg#(Bit#(2)) ya <- mkReg(0);
   Reg#(Bit#(2)) yb <- mkReg(0);
   Reg#(Bit#(4)) lo <- mkReg(0);
   Reg#(Bool) sel <- mkReg(False);
   Reg#(Bool) pick <- mkReg(False);
   Reg#(Knob) k1 <- mkReg(unpack(0));
   Reg#(Knob) k2 <- mkReg(unpack(0));
   Reg#(Bit#(1)) ta <- mkReg(0);
   Reg#(Bit#(1)) tb <- mkReg(0);
   Reg#(Bit#(2)) ua <- mkReg(0);
   Reg#(Bit#(2)) ub <- mkReg(0);

   // the chr applied to the conditional, pushed into its arms
   rule pushed;
      Bit#(1) a = xa;
      Bit#(1) b = xb;
      Bit#(2) c = ya;
      Bit#(2) d = yb;
      Knob k = Knob { lo: lo, st: unpack(sel ? a : b), q: unpack(pick ? c : d) };
      k1 <= k;
      ta <= a;
      tb <= b;
      ua <= c;
      ub <= d;
   endrule

   // a chr in each arm, merged above the conditional
   rule merged;
      Bit#(1) a = xa;
      Bit#(1) b = xb;
      Bit#(2) c = ya;
      Bit#(2) d = yb;
      Knob k = Knob { lo: lo, st: sel ? unpack(a) : unpack(b), q: pick ? unpack(c) : unpack(d) };
      k2 <= k;
      ta <= a;
      tb <= b;
      ua <= c;
      ub <= d;
   endrule

   method Bit#(7) out();
      return pack(k1) ^ pack(k2);
   endmethod
endmodule
