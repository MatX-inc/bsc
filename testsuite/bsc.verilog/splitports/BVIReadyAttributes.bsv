// The ready port written in a BVI declaration controls imported readiness,
// even when the source interface carries synthesis-only always_ready metadata.
interface MethodReadyAttribute;
   (* always_ready *)
   method Bit#(8) read;
endinterface

(* always_ready *)
interface InterfaceReadyAttribute;
   method Bit#(8) read;
endinterface

import "BVI" BVIReadyForeign =
module mkMethodReadyAttribute(MethodReadyAttribute);
   default_clock no_clock;
   default_reset no_reset;
   method VALUE read ready(READY);
   schedule read CF read;
endmodule

import "BVI" BVIReadyForeign =
module mkInterfaceReadyAttribute(InterfaceReadyAttribute);
   default_clock no_clock;
   default_reset no_reset;
   method VALUE read ready(READY);
   schedule read CF read;
endmodule

(* synthesize *)
module sysBVIReadyAttributes(Empty);
   MethodReadyAttribute methodAttribute <- mkMethodReadyAttribute;
   InterfaceReadyAttribute interfaceAttribute <- mkInterfaceReadyAttribute;
   Reg#(UInt#(3)) cycle <- mkReg(0);

   rule readMethod;
      $display("FAIL: method always_ready ignored BVI ready: %0d", methodAttribute.read);
   endrule
   rule readInterface;
      $display("FAIL: interface always_ready ignored BVI ready: %0d", interfaceAttribute.read);
   endrule
   rule finish;
      cycle <= cycle + 1;
      if (cycle == 3) begin
         $display("BVI ready ports override synthesis attributes");
         $finish(0);
      end
   endrule
endmodule
