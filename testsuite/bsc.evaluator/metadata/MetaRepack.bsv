// Views and validity: bits reinterpreted as types.  The register's
// value is unpacked as the Flag enumeration it was declared with (the
// unpack of a port constant is a declaration, not a repack); a byte
// register's bits are unpacked as a Flag they are not known to be
// valid for (a deliberate repack, recorded on the cell of the bits);
// bits unpacked as UInt are hole-free, so no repack; a struct is
// packed from fields that are all valid, so its bits are valid by
// construction; and the fields moved out of a struct read from a
// method carry the struct's path.
typedef enum { Idle, Busy, Done } Flag deriving (Bits, Eq);

typedef struct {
   Bit#(4) lo;
   Flag    st;
} Pair deriving (Bits, Eq);

interface RepackIfc;
   method Bit#(8) out();
   method Action put(Bit#(8) x);
endinterface

(* synthesize *)
module sysMetaRepack(RepackIfc);
   Reg#(Flag) fl <- mkReg(Idle);
   Reg#(Bit#(8)) raw <- mkReg(0);
   Reg#(UInt#(8)) cnt <- mkReg(0);
   Reg#(Pair) pr <- mkReg(Pair { lo: 0, st: Idle });
   Reg#(Bit#(2)) tag <- mkReg(0);

   // the enumeration read back from its own register: declared valid
   rule advance (fl == Idle);
      fl <= Busy;
   endrule

   // a byte's low bits reinterpreted as the enumeration: a repack,
   // recorded on the cell of the bits, which survives as a def because
   // the bits are also written to a register
   rule reinterpret;
      Bit#(2) low = raw[1:0];
      Flag f = unpack(low);
      tag <= low;
      if (f == Done)
         raw <= 0;
      else
         raw <= raw + 1;
   endrule

   // bits unpacked as a hole-free type: no repack
   rule count;
      cnt <= unpack(raw) + cnt;
   endrule

   // a struct packed from valid fields, and its fields moved out
   rule pair;
      Pair p = pr;
      pr <= Pair { lo: p.lo + 1, st: p.st };
   endrule

   method Bit#(8) out();
      return raw ^ pack(cnt);
   endmethod

   method Action put(Bit#(8) x);
      raw <= x;
   endmethod
endmodule
