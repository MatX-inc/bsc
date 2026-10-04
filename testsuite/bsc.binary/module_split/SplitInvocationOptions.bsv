package SplitInvocationOptions;

interface InvocationOptions;
   method Bit#(8) undetermined;
endinterface

// Keep an undefined value observable so -unspecified-to changes backend output
// without changing elaboration or scheduling.
(* synthesize *)
module sysSplitInvocationOptions(InvocationOptions);
   method Bit#(8) undetermined = ?;
endmodule

endpackage
