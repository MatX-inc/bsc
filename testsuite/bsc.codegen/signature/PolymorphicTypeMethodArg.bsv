interface IFC#(type t);
   method Action m (t x);
endinterface

(* synthesize *)
module sysPolymorphicTypeMethodArg (IFC#(t));
   method Action m (t x);
   endmethod
endmodule
