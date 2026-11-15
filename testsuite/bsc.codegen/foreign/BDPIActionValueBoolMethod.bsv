import BDPIActionValueImport::*;

interface BoolMethod;
   method ActionValue#(Bool) m(Bool c);
endinterface

(* synthesize *)
module mkBDPIActionValueBoolMethod (BoolMethod);
   // the result of the method is the join of a Bool result with its
   // default, c && b
   method ActionValue#(Bool) m(Bool c);
      Bool b = False;
      if (c) b <- my_flag(1);
      return b;
   endmethod
endmodule
