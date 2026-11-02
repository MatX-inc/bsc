interface Rel;
   method Action  setA(Bit#(8) x);
   method Action  incA;
   method Action  setB(Bit#(8) x);
   method Bit#(8) getA;
   method Bit#(8) getB;
endinterface

// Exported scheduling relations among the module's own methods.
// SchedInfo as computed by bsc 2026.07-6 (-dvschedinfo):
//   getA SB [incA, setA];  getB SB setB          value read before write
//   incA C incA                                  read-modify-write
//   [incA, setA] SBR [setA, setB];  [getA, setB] SBR setB
//   getA CF [getA, getB];  [getB, incA, setA] CF getB
//   rulesBetweenMethods = [((setA,setB),[RL_mv]), ((incA,setB),[RL_mv]),
//                          ((getA,setB),[RL_mv])]
//   rulesBeforeMethods  = [(setB, [Left RL_mv])]
// The internal rule mv (a <= b) has to execute after getA/incA/setA and
// before setB, which turns setA/setB (otherwise CF) into SBR.  Execution
// order: getA, incA, setA, getB, mv, setB; mv's write of a shadows setA's
// and incA's (G0117).
//
// Compile: bsc -u -verilog -elab -show-schedule MethodRelations.bsv
(* synthesize *)
module mkMethodRelations(Rel);
   Reg#(Bit#(8)) a  <- mkReg(0);
   Reg#(Bit#(8)) b  <- mkReg(0);
   Reg#(Bool)    on <- mkReg(False);

   rule mv (on);
      a <= b;
   endrule

   method Action setA(Bit#(8) x);
      a <= x;
   endmethod

   method Action incA;
      a <= a + 1;
   endmethod

   method Action setB(Bit#(8) x);
      b  <= x;
      on <= x[0] == 1;
   endmethod

   method Bit#(8) getA = a;
   method Bit#(8) getB = b;
endmodule
