typedef union tagged {
   void Red;
   Bit#(8) Green;
   Bool Blue;
} Color deriving (Bits, Eq);

typedef enum { X, Y, Z, W, V } Five deriving (Bits, Eq);

typedef struct {
   Bool a;
   Bit#(4) b;
} S deriving (Bits, Eq);

// all constructors covered
function Bit#(8) allArms(Color c);
   return (case (c) matches
              tagged Red: 0;
              tagged Green .g: g;
              tagged Blue .b: (b ? 1 : 0);
           endcase);
endfunction

// wildcard covers the rest
function Bit#(8) hasWildcard(Color c);
   return (case (c) matches
              tagged Green .g: g;
              .*: 0;
           endcase);
endfunction

// default covers the rest
function Bit#(8) hasDefault(Color c);
   return (case (c) matches
              tagged Green .g: g;
              default: 0;
           endcase);
endfunction

// all 2^n literals of a sized type is a complete match
function Bit#(4) allBits(Bit#(1) x);
   return (case (x) matches
              0: 5;
              1: 6;
           endcase);
endfunction

// struct fields covered by the variable pattern in the second arm
function Bit#(4) structArms(S s);
   return (case (s) matches
              S { a: True, b: .bb }: bb;
              S { b: .bb }: bb + 1;
           endcase);
endfunction

// interface using pattern matching statements and plain case statements;
// also exercises deriving on a non-power-of-2 enum (the generated
// unpack function is intentionally incomplete and must not warn)
interface Ifc;
   method Bit#(4) get();
endinterface

(* synthesize *)
module mkPatternCheckStmts(Ifc);
   Reg#(Color) c <- mkRegU;
   Reg#(Five) f <- mkRegU;
   Reg#(Bit#(4)) o <- mkRegU;

   rule r1;
      // statement-form case..matches without a default is not a warning
      // (unmatched values simply perform no action)
      case (c) matches
         tagged Green .g: o <= truncate(g);
      endcase
   endrule

   // pattern matching in rule conditions is a filter, not a warning
   rule r2 (c matches tagged Blue .b &&& b);
      o <= 1;
   endrule

   rule r3;
      // plain case statements are not pattern matching
      case (f)
         X: o <= 1;
         Y: o <= 2;
      endcase
   endrule

   rule r4;
      // if with a pattern condition is a filter, not a warning
      if (c matches tagged Red)
         o <= 2;
   endrule

   method Bit#(4) get();
      return o;
   endmethod
endmodule
