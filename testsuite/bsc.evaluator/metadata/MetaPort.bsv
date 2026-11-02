// A method-call port def: two rules and a method write register a with
// different values, so aState builds a mux for the register's write
// port from the three calls (its selector and value defs are created
// from the rules and the method, the port def from all three), and
// aRenameIO renames the port def to the Verilog port name.
interface PortIfc;
   method Action put(UInt#(8) x);
endinterface

(* synthesize *)
module sysMetaPort(PortIfc);
   Reg#(UInt#(8)) a <- mkReg(0);
   Reg#(UInt#(8)) b <- mkReg(0);
   Reg#(Bool) c <- mkReg(False);

   rule up (c);
      a <= a + b;
   endrule

   rule down (!c);
      a <= a - 1;
   endrule

   method Action put(UInt#(8) x);
      a <= x;
      c <= !c;
   endmethod
endmodule
