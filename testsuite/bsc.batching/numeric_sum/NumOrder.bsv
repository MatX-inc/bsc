package NumOrder;

// zeroExtend from one bit to n + (m + 2) bits leaves the typechecker to
// solve Add#(1, k, TAdd#(n, TAdd#(m, 2))) for k.  The built-in Add rules
// cancel the common terms of the two sides and rebuild the remainder as a
// sum, and that sum's term order is the type that k is bound to.  The
// first two arguments only pin n and m.
function Bit#(TAdd#(n, TAdd#(m, 2))) numOrder(Bit#(n) a, Bit#(m) b, Bit#(1) x);
   return zeroExtend(x);
endfunction

endpackage
