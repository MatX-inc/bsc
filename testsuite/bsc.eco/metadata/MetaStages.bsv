import Vector::*;

// A vector of identical pipeline stages: every stage's next value is
// the same function of the previous stage, built by one iteration of
// one static loop, so the stages' records differ only by the loop
// count on their chains.  Each next value is written to the stage and
// to its shadow, so that the value stays a def of its own through the
// passes instead of being inlined into the rule.  OLD is the module as
// written; NEW (-D ECO_EDIT) edits stage 2 alone by giving it another
// function from the vector of per-stage edits, so that the rule's body
// and the sites of the other stages are the same in both and of the
// four stage records three pair across the captures and one does not.
interface StagesIfc;
   method Bit#(8) last();
endinterface

function Bit#(8) step(Bit#(8) x);
   return x + 1;
endfunction

function Bit#(8) same(Bit#(8) x);
   return x;
endfunction

function Bit#(8) twist(Bit#(8) x);
   return x ^ 8'h55;
endfunction

(* synthesize *)
module mkMetaStages(StagesIfc);
   Vector#(4, Reg#(Bit#(8))) stages <- replicateM(mkRegU);
   Vector#(4, Reg#(Bit#(8))) shadow <- replicateM(mkRegU);

   Vector#(4, function Bit#(8) f(Bit#(8) x)) edits = replicate(same);
`ifdef ECO_EDIT
   edits[2] = twist;
`endif

   rule advance;
      for (Integer i = 0; i < 4; i = i + 1) begin
         let v = edits[i](step(stages[(i + 3) % 4]));
         stages[i] <= v;
         shadow[i] <= v;
      end
   endrule

   method Bit#(8) last() = stages[3];
endmodule
