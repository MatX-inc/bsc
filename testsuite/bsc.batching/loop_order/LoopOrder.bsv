package LoopOrder;

// A loop that updates two variables: the desugaring threads them through a
// tuple whose component order is the order in which it lists the updated
// variables.
function Bit#(8) loopOrder(Bit#(8) x);
   Bit#(8) zz = 0;
   Bit#(8) aa = x;
   for (Integer i = 0; i < 4; i = i + 1) begin
      zz = zz + aa;
      aa = aa + 1;
   end
   return zz + aa;
endfunction

endpackage
