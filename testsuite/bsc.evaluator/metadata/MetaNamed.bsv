// A merge in aConv where the source name won over the CSE name: the
// argument of blend is bound to its parameter y and used twice, so its
// def survives the ISyntax passes under the parameter's name; the
// conversion names the CSE def of its expression from the expression
// (__d<n>), merges the source def into it and then gives the CSE def
// the source name back.
function Bit#(8) blend(Bit#(8) x, Bit#(8) y);
   return (x & y) | (y << 1);
endfunction

(* synthesize *)
module sysMetaNamed(Empty);
   Reg#(Bit#(8)) a <- mkReg(0);
   Reg#(Bit#(8)) b <- mkReg(0);

   rule step;
      a <= blend(a, b ^ 8'h33);
   endrule
endmodule
