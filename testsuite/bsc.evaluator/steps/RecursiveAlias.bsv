// A recursive function that calls itself through a local alias, used
// twice so that it is bound rather than substituted.  The alias is a
// reference to the function, so every call through it is an unfolding
// of the function and counts: a chain of 30 fits in a budget of 400
// steps.

function Integer loop2(Integer x);
   let l = loop2;
   return (x == 0) ? 0 : ((x > 0) ? 1 + l(x - 1) : l(x + 1));
endfunction

(* synthesize *)
module sysRecursiveAlias();
   rule r1;
      $display(loop2(30));
      $finish(0);
   endrule
endmodule
