// Derived views.  The rebuild derives the paths a value has in the
// source types from the move edges the cells carry (the parent cell
// and the field or constructor under the parent's type) and the
// types: a leaf three structs deep reads back the path from each of
// its ancestors; the payloads of two constructors of one payload type
// are told apart by the constructor's name; a struct payload
// extracted from a Maybe read from a register reads its field (the
// Valid constructor under the unpack's conditional is cancelled
// against the match before it lands anywhere, so no path runs
// through it); a chain of a recursive type is cut at that type, so
// every element reads one unrolling of the Link path whatever its
// depth; the field selected from a conditional of two structs reads
// the field alone (an operand of the conditional is no field); and a
// struct packed from fresh field cells reads the struct's type from
// the concatenation of its fields.
typedef struct {
   Bit#(4) x;
   Bit#(2) y;
} Inner deriving (Bits, Eq);

typedef struct {
   Bit#(5) b;
   Inner   i;
} Mid deriving (Bits, Eq);

typedef struct {
   Bit#(3) a;
   Mid     m;
} Outer deriving (Bits, Eq);

typedef union tagged {
   Bit#(8) A;
   Bit#(8) B;
} T deriving (Bits, Eq);

typedef struct {
   Bit#(8) lo;
   Bit#(8) hi;
} Word deriving (Bits, Eq);

// a recursive type: a chain of bytes, built by nesting its constructor
// and consumed by a recursive function
typedef union tagged {
   void Last;
   struct {
      Bit#(8) hd;
      Chain   tl;
   } Link;
} Chain;

function Bit#(8) total(Chain c);
   case (c) matches
      tagged Last: return 0;
      tagged Link { hd: .h, tl: .t }: return h + total(t);
   endcase
endfunction

interface DerivedIfc;
   method Bit#(4) leaf();
   method Action put(Bit#(8) v);
endinterface

(* synthesize *)
module sysMetaDerived(DerivedIfc);
   Reg#(Outer) outr <- mkReg(unpack(0));
   Reg#(Bit#(4)) xr <- mkReg(0);
   Reg#(Bit#(4)) xm <- mkReg(0);
   Reg#(Bit#(8)) raw <- mkReg(0);
   Reg#(Bit#(8)) acc <- mkReg(0);
   Reg#(Maybe#(Inner)) mi <- mkReg(tagged Invalid);
   Reg#(Bit#(8)) sum3 <- mkReg(0);
   Reg#(Bit#(8)) sel <- mkReg(0);
   Reg#(Bit#(16)) wide <- mkReg(0);

   // three structs deep: the leaf x of the Inner inside the Mid inside
   // the Outer read from a register
   rule nest;
      Outer o = outr;
      xr <= o.m.i.x + 1;
   endrule

   // two constructors of one payload type, each applied to register
   // bits in a value of its own and matched (a constructor applied
   // under a conditional is cancelled against the match before it
   // lands anywhere: no cell, no edge)
   rule choose_tag;
      T ta = tagged A (raw + 1);
      T tb = tagged B (raw - 1);
      case (ta) matches
         tagged A .av: acc <= av;
         tagged B .bv: acc <= ~bv;
      endcase
      case (tb) matches
         tagged A .av: sel <= ~av;
         tagged B .bv: sel <= bv;
      endcase
   endrule

   // a struct payload extracted from a Maybe read from a register
   rule valid_inner;
      if (mi matches tagged Valid .inn)
         xm <= inn.x;
   endrule

   // two chains of different lengths, each element a fresh value,
   // summed by the recursive function: the cut at the recursive type
   // gives every element one unrolling of the path, whatever its depth
   rule chains;
      Chain c2 = tagged Link { hd: raw + 1,
                 tl: tagged Link { hd: raw + 2, tl: tagged Last } };
      Chain c4 = tagged Link { hd: raw + 1,
                 tl: tagged Link { hd: raw + 2,
                 tl: tagged Link { hd: raw + 3,
                 tl: tagged Link { hd: raw + 4, tl: tagged Last } } } };
      sum3 <= total(c2) ^ total(c4);
   endrule

   // a conditional of two struct values, one field selected
   rule choose_word;
      Word w = (raw[1] == 1) ? Word { lo: raw, hi: acc }
                             : Word { lo: acc, hi: raw };
      sel <= w.lo + sel;
   endrule

   // a struct packed from fresh field cells into a Bit value, one of
   // them not known valid (an exclusive or), so that the struct's type
   // comes from the concatenation of the fields and not from the
   // validity
   rule pack_word;
      Word w2 = Word { lo: raw ^ acc, hi: acc + 3 };
      Bit#(16) bits = pack(w2);
      wide <= bits + zeroExtend(raw);
   endrule

   method Bit#(4) leaf();
      return xr ^ xm;
   endmethod

   method Action put(Bit#(8) v);
      raw <= v;
   endmethod
endmodule
