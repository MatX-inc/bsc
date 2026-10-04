package SplitModule;

interface Counter;
   method Action put(Bit#(8) value);
   method Bit#(8) read;
endinterface

(* synthesize *)
module mkSplitCounter(Counter);
`ifdef SPLIT_VARIANT
   Reg#(Bit#(8)) value <- mkReg(19);
`else
   Reg#(Bit#(8)) value <- mkReg(7);
`endif

   method Action put(Bit#(8) next);
      value <= next;
   endmethod

   method Bit#(8) read = value;
endmodule

(* synthesize *)
module sysSplitModule(Empty);
   Counter counter <- mkSplitCounter;
   Reg#(Bit#(8)) cycle <- mkReg(0);

   rule advance (cycle < 4);
      counter.put(cycle + 20);
      cycle <= cycle + 1;
   endrule

   rule finish (cycle == 4);
      $display("split-result: %0d", counter.read);
      $finish(0);
   endrule
endmodule

endpackage
