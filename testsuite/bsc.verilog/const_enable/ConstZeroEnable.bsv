// A resettable register that no rule or method ever writes: its enable
// resolves to the constant 0, so the update can never happen.
//
// "live" is here so a mangled always block is distinguishable from the
// intended removal of one register's update.
interface ConstZeroEnable;
   method Bit#(8) result;
endinterface

(* synthesize *)
module mkConstZeroEnable (ConstZeroEnable);
   Reg#(Bit#(8)) live   <- mkReg(0);
   Reg#(Bit#(8)) frozen <- mkReg(8'hAB);

   rule go;
      live <= live + 1;
   endrule

   // frozen is read, so it is not dead state -- only its enable is constant
   method result = live + frozen;
endmodule
