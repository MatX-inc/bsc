typedef union tagged {
   void Red;
   Bit#(8) Green;
   Bool Blue;
} Color deriving (Bits, Eq);

// a guarded arm does not shadow a later arm with the same pattern,
// and the unguarded arms cover everything: no warnings at all
function Bit#(8) guardedNotShadowing(Color c, Bool sel);
   return (case (c) matches
              tagged Green .g &&& sel: g;
              tagged Green .g: g + 1;
              tagged Red: 0;
              tagged Blue .*: 1;
           endcase);
endfunction

// an arm whose pattern is subsumed is dead even if the arm is guarded
function Bit#(8) guardedButDead(Color c, Bool sel);
   return (case (c) matches
              tagged Green .*: 0;
              tagged Green .g &&& sel: g;
              default: 1;
           endcase);
endfunction

interface Ifc;
   method ActionValue#(Bit#(8)) get(Color c);
   method Bit#(8) peek(Color c);
endinterface

(* synthesize *)
module mkGuardsCond(Ifc);
   Reg#(Maybe#(Bit#(8))) mr <- mkReg(tagged Invalid);
   Reg#(Bit#(8)) o <- mkRegU;

   // chained pattern conditions with &&& are filters: no warnings
   rule r1 (mr matches tagged Valid .v &&& v > 5);
      o <= v;
   endrule

   rule r2;
      // conditional expression whose test is a pattern match: no warnings
      let x = mr matches tagged Valid .* ? o : 0;
      // if-else with pattern condition and boolean guard: no warnings
      if (mr matches tagged Valid .v &&& v == 0)
         o <= x;
      else
         o <= x + 1;
   endrule

   // a method with an implicit condition; the complete match inside
   // must not warn
   method ActionValue#(Bit#(8)) get(Color c) if (mr matches tagged Valid .*);
      mr <= tagged Invalid;
      return (case (c) matches
                 tagged Red: 0;
                 tagged Green .g: g;
                 tagged Blue .*: 1;
              endcase);
   endmethod

   // an implicit condition does not make an incomplete match complete
   method Bit#(8) peek(Color c) if (isValid(mr));
      return (case (c) matches
                 tagged Red: 0;
                 tagged Green .g: g;
              endcase);
   endmethod
endmodule
