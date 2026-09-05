// P0a proposed Verilog-parity witness; NOT yet compiler/oracle validated.
// Shape from the pinned SchedCondsDynamicSchedule.bsv, initialized here.
// first/second CAN_FIREs are NOT disjoint: both parent rules execute.
// Child order: early < relay < late. Parent order: first < second.
// c=True requires relay before first; c=False requires relay after second.
// An ADynSched-only extractor does not cover this Verilog-target shape.

interface Boundary;
   method Action early();
   method Action late();
   method Bit#(24) sample();
endinterface

(* synthesize *)
module mkDynamicConditionalCallsChild(Boundary);
   Reg#(Bit#(8)) source <- mkReg(7);
   Reg#(Bit#(8)) middle <- mkReg(3);
   Reg#(Bit#(8)) captured <- mkReg(0);

   rule relay;
      middle <= source;
   endrule

   method Action early();
      captured <= middle;
   endmethod
   method Action late();
      source <= 0;
   endmethod
   method Bit#(24) sample();
      return {source, middle, captured};
   endmethod
endmodule

(* synthesize *)
module sysDynamicConditionalCalls();
   Boundary child <- mkDynamicConditionalCallsChild;
   Reg#(Bit#(8)) source <- mkReg(9);
   Reg#(Bit#(8)) copied <- mkReg(0);
   Reg#(Bool) c <- mkReg(True);
   Reg#(UInt#(8)) cycle <- mkReg(0);

   rule first;
      copied <= source;
      if (c) child.late;
   endrule

   rule second;
      source <= 0;
      if (!c) child.early;
   endrule

   rule observe;
      $display("P0 c=%0d cycle=%0d parent=%0d,%0d child=%06h",
               pack(c), cycle, source, copied, child.sample);
   endrule

   rule advance;
      c <= !c;
      cycle <= cycle + 1;
   endrule

   rule finish (cycle == 6);
      $finish(0);
   endrule
endmodule
