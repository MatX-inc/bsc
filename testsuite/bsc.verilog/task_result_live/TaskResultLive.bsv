import "BDPI" function ActionValue#(Bit#(8)) fetch_value();

// The imported function's result is assigned inside the foreign-function
// block but read by a register input, which is synthesized logic.
interface TaskResultLive;
   method Bit#(8) result;
endinterface

(* synthesize *)
module mkTaskResultLive (TaskResultLive);
   Reg#(Bit#(8)) held <- mkReg(0);

   rule go;
      let v <- fetch_value();
      held <= v;
   endrule

   method result = held;
endmodule
