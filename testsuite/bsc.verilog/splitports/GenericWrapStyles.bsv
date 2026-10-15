import GenericWrapTypes::*;

(* synthesize, always_ready = "leaf.read,leaf.translate,snapshot" *)
module mkGenericWrapSynth(Tree#(Payload));
   Reg#(Payload) stored <- mkReg(Payload { lo: 'h12, hi: 'h34 });
   Reg#(Bool) ready <- mkReg(False);

   rule toggle_ready;
      ready <= !ready;
   endrule

   interface Leaf leaf;
      method read = stored;
      method Action put(Payload x) if (ready);
         stored <= x;
      endmethod
      method ActionValue#(Payload) exchange(Payload x) if (ready);
         stored <= x;
         return stored;
      endmethod
      method Payload translate(Payload x);
         return Payload { lo: x.lo ^ 'hf0, hi: x.hi ^ 'h0f };
      endmethod
   endinterface
   method snapshot = stored;
endmodule

import "BVI" GenericWrapForeign =
module mkGenericWrapBVI(Tree#(a)) provisos (Bits#(a, n));
   default_clock clk(CLK);
   default_reset rst(RST_N);
   parameter WIDTH = valueOf(n);

   // BVI declarations need not be in the source interface's field order.
   method SNAPSHOT snapshot();
   interface Leaf leaf;
      method TRANSLATE_RESULT translate(TRANSLATE);
      method EXCHANGE_RESULT exchange(EXCHANGE) enable(EN_EXCHANGE) ready(READY);
      method put(PUT) enable(EN_PUT) ready(READY);
      method READ read();
   endinterface

   schedule (leaf.read, leaf.translate, snapshot) CF
            (leaf.read, leaf.translate, snapshot);
   schedule (leaf.read, snapshot) SB (leaf.put, leaf.exchange);
   schedule leaf.translate CF (leaf.put, leaf.exchange);
   schedule (leaf.put, leaf.exchange) C (leaf.put, leaf.exchange);
endmodule

(* synthesize *)
module sysGenericWrapStyles(Empty);
   Tree#(Payload) generated <- mkGenericWrapSynth;
   Tree#(Payload) imported <- mkGenericWrapBVI;
   Reg#(UInt#(8)) cycle <- mkReg(0);
   Reg#(UInt#(2)) step <- mkReg(0);

   rule tick;
      cycle <= cycle + 1;
      if (cycle == 20) begin
         $display("FAIL: timeout waiting for wrapped methods");
         $finish(1);
      end
   endrule

   rule put (step == 0);
      Payload old = Payload { lo: 'h12, hi: 'h34 };
      Payload next = Payload { lo: 'hab, hi: 'hcd };
      if (cycle != 1 || generated.leaf.read != old || imported.leaf.read != old)
         $display("FAIL: initial value or put readiness");
      generated.leaf.put(next);
      imported.leaf.put(next);
      step <= 1;
   endrule

   rule exchange (step == 1);
      Payload old = Payload { lo: 'hab, hi: 'hcd };
      Payload next = Payload { lo: 'hfe, hi: 'hdc };
      Payload translated = Payload { lo: 'h5b, hi: 'hc2 };
      let generatedOld <- generated.leaf.exchange(next);
      let importedOld <- imported.leaf.exchange(next);
      if (cycle != 3 || generatedOld != old || importedOld != old ||
          generated.snapshot != old || imported.snapshot != old ||
          generated.leaf.translate(old) != translated ||
          imported.leaf.translate(old) != translated)
         $display("FAIL: argument/result layout or exchange readiness");
      step <= 2;
   endrule

   rule finish (step == 2);
      Payload expected = Payload { lo: 'hfe, hi: 'hdc };
      if (cycle != 4 || generated.leaf.read != expected ||
          imported.leaf.read != expected || generated.snapshot != expected ||
          imported.snapshot != expected)
         $display("FAIL: final value after exchange");
      $display("generic synthesis and BVI wrapping agree");
      $finish(0);
   endrule
endmodule
