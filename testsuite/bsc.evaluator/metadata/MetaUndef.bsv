// Undefined values: a user don't-care under a dynamic condition reaches
// the conditional's def, where the metadata table records its kind
// (dont-care); the def's site and context are the origin.  The struct
// field's don't-care folds into the register write and reaches no def.
typedef struct {
   Bit#(8) a;
   Bit#(8) b;
} Two deriving (Bits, Eq);

interface UndefIfc;
   method Bit#(8) out();
endinterface

(* synthesize *)
module sysMetaUndef(UndefIfc);
   Reg#(Bit#(8)) r <- mkReg(0);
   Reg#(Two) t <- mkReg(Two { a: 0, b: 0 });
   Reg#(Bool) c <- mkReg(False);

   // a user don't-care under a dynamic condition
   rule dc;
      Bit#(8) v = c ? r + 1 : ?;
      r <= v;
   endrule

   // a struct whose field is left undefined
   rule part;
      Two u = Two { a: r, b: ? };
      t <= u;
   endrule

   method Bit#(8) out();
      return t.a;
   endmethod
endmodule
