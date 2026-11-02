package MetaCrossLib;

// A function called from inside its own package (viaScale) and from
// another package (the test): the call inside carries the definition's
// own position, the call from outside its own.
function UInt#(8) incr(UInt#(8) x);
   return x + 1;
endfunction

function UInt#(8) scale(UInt#(8) x);
   return incr(x + x);
endfunction

function UInt#(8) viaScale(UInt#(8) x);
   return incr(scale(x));
endfunction

endpackage
