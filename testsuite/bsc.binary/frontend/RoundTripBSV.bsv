// Constructs whose signatures and typechecked forms the format 2
// encoding must round-trip (see frontend.exp)

import Vector :: *;

typedef enum { Idle, Busy, Done } State deriving (Bits, Eq);

typedef struct {
   Bit#(8) tag;
   Maybe#(UInt#(16)) payload;
} Packet deriving (Bits, Eq);

typedef union tagged {
   void Nop;
   Bit#(4) Small;
   Packet Big;
} Msg deriving (Bits, Eq);

interface Stage;
   method Action put(Msg m);
   method State state();
endinterface

function Bit#(8) msgTag(Msg m);
   case (m) matches
      tagged Nop: return 0;
      tagged Small .s: return zeroExtend(s);
      tagged Big .p: return p.tag;
   endcase
endfunction

function Bit#(32) total(Vector#(4, Bit#(8)) v);
   Bit#(32) acc = 0;
   for (Integer i = 0; i < 4; i = i + 1)
      acc = acc + zeroExtend(v[i]);
   return acc;
endfunction

(* synthesize *)
module sysRoundTripBSV(Stage);
   Reg#(State) st <- mkReg(Idle);
   Reg#(Bit#(8)) last <- mkReg(0);

   rule finish (st == Busy);
      st <= Done;
   endrule

   method Action put(Msg m) if (st != Busy);
      last <= msgTag(m);
      st <= Busy;
   endmethod

   method State state() = st;
endmodule
