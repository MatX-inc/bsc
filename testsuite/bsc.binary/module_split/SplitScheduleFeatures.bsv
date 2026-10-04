package SplitScheduleFeatures;

// Keep a call nested in an expression so materialization must lift it and
// assign the same instance name for direct compilation and artifact reload.
(* noinline *)
function Bit#(8) splitAddThree(Bit#(8) value);
   return value + 3;
endfunction

interface SplitSlot;
   method Action load(Bit#(8) value);
   method ActionValue#(Bit#(8)) take;
   method Bit#(8) peek;
endinterface

(* synthesize *)
module mkSplitSlot(SplitSlot);
   Reg#(Bool) full <- mkReg(False);
   Reg#(Bit#(8)) data <- mkRegU;

   method Action load(Bit#(8) value) if (!full);
      data <= value;
      full <= True;
   endmethod

   method ActionValue#(Bit#(8)) take if (full);
      full <= False;
      return data;
   endmethod

   // The undefined branch is never observed by the test, but must survive
   // the scheduling checkpoint and be handled by backend materialization.
   method Bit#(8) peek = full ? data : ?;
endmodule

(* synthesize *)
module sysSplitScheduleFeatures(Empty);
   SplitSlot slot <- mkSplitSlot;
   Reg#(Bit#(8)) cycle <- mkReg(0);
   Reg#(Bit#(8)) shared <- mkReg(0);
   Reg#(Bit#(8)) marks <- mkReg(0);
   Reg#(Bit#(8)) total <- mkReg(0);

   rule advance (cycle < 4);
      cycle <= cycle + 1;
   endrule

   // Both rules fire, but their calls to the shared register are conditional
   // and disjoint. This exercises conflict-free condition wires and checks.
   (* conflict_free = "updateEven, updateOdd" *)
   rule updateEven (cycle < 4);
      if (cycle[0] == 0)
         shared <= shared + 1;
   endrule

   rule updateOdd (cycle < 4);
      if (cycle[0] == 1)
         shared <= shared + 2;
   endrule

   // Scheduling adds a monitor rule for this asserted relationship.
   (* mutually_exclusive = "markEven, markOdd" *)
   rule markEven (cycle < 4 && cycle[0] == 0);
      marks <= marks + 3;
   endrule

   rule markOdd (cycle < 4 && cycle[0] == 1);
      marks <= marks + 5;
   endrule

   rule loadSlot (cycle < 4 && cycle[0] == 0);
      slot.load(splitAddThree(cycle) + 1);
   endrule

   rule takeSlot (cycle < 4 && cycle[0] == 1);
      let value <- slot.take;
      total <= total + value;
   endrule

   rule finish (cycle == 4);
      $display("split-features: %0d %0d %0d", shared, marks, total);
      $finish(0);
   endrule
endmodule

endpackage
