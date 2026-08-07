// Pin for the corner where the guarded and unguarded given-checks
// disagree in the OTHER direction: judged eagerly (unguarded), the
// given CC#(4,8) clashes on literals once r is substituted and is
// provably unreachable; judged guarded, both numeric positions defer
// as equalities ((r,4),(r,8)) whose conjunction is unsatisfiable, yet
// a bare isJust would count them "unifiable" and bar the commit.
//
// Today the program is accepted on every path for a prior reason:
// the diagonal instance's Mul proviso is a SAT validity in w, so the
// settlement batch proves and erases it AT THE INSTANCE DECLARATION,
// and the use-site goal CC#(r,r) reduces completely -- the gate never
// judges a partial reduction here.  This file pins that acceptance:
// if instance-proviso erasure or the gate's deferred-equality
// handling ever changes, an unreachable contradictory given must
// still not reject this program.
typeclass CC#(numeric type a, numeric type b);
   function Bit#(b) ccv(Bit#(a) x);
endtypeclass

instance CC#(w, w)
   provisos (Mul#(TDiv#(w,8), 8,
                  TAdd#(TSub#(TMul#(TDiv#(w,8), 8), w), w)));
   function ccv(x) = x;
endinstance

function Bit#(r) f(Bit#(r) x) provisos (CC#(4,8));
   return ccv(x);
endfunction
