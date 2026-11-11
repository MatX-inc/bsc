package RedundantProvisosBSV;

function Bool lessThan(a x, a y) provisos (Eq#(a), Ord#(a));
   return x < y;
endfunction

module mkStore#(a init)(Reg#(a)) provisos (Bits#(a, sa), Arith#(a));
   Reg#(a) r <- mkReg(init);
   return r;
endmodule

endpackage
