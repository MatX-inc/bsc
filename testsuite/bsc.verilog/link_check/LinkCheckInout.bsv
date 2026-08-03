// A shorted boundary inout: the module argument is re-exported as an
// interface inout, so two header ports share one net and the generated
// header keeps the port-expression form (".X1(net), .X2(net)"), which
// verilator cannot parse.  bsc records the fact in the .v header and
// the verilator builder refuses the link (and check) up front.

interface LCInoutIfc;
   interface Inout#(int) i_out;
endinterface

(* synthesize *)
module sysLCInoutShort #(Inout#(int) i_in) (LCInoutIfc);
   interface i_out = i_in;
endmodule
