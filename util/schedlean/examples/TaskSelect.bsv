// A system task that reads every copy of a vector of submodules but prints
// only one: bsc's testsuite design SBRValueOK_small, smaller.
//  * mkCounter has readCount SBR incCount: its rule runs between the two
//  * test prints counters[j].readCount and calls counters[j + 1].incCount;
//    the display's selection reads every copy, so each copy's read is out
//    of order next to that copy's call, but the copy it prints is j's, which
//    the rule does not increment: legal, which the theorem shows by
//    following the selection
//
// Compile: bsc -u -verilog -elab TaskSelect.bsv
import Vector::*;

interface Count;
   method Bit#(26) readCount;
   method Action incCount;
endinterface

(* synthesize *)
module mkCounter(Count);
   Reg#(Bit#(26))  counter     <- mkReg(0);
   Wire#(Bit#(26)) counterWire <- mkWire;

   rule driveCounter;
      counterWire <= counter;
      counter <= 0;
   endrule

   method readCount = counter;

   method Action incCount;
      counter <= counterWire + 1;
   endmethod
endmodule

(* synthesize *)
module mkTaskSelect(Empty);
   Vector#(4, Count) counters <- replicateM(mkCounter);
   Reg#(UInt#(2))    j        <- mkReg(0);

   rule test;
      $display(counters[j].readCount);
      counters[j + 1].incCount;
      j <= j + 1;
   endrule
endmodule
