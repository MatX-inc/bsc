// A class method that calls itself through a user wrapper around the
// method.  The recursion is deeper than the test's budget of
// unfolding steps, so elaboration must stop with G0024.

typeclass Count#(type a);
   function Integer count(a x);
endtypeclass

function Integer countVia(Integer x) = count(x);

instance Count#(Integer);
   function Integer count(Integer x) = (x == 0) ? 0 : 1 + countVia(x - 1);
endinstance

(* synthesize *)
module sysRecursiveMethodWrapper();
   rule r1;
      $display(count(1000));
      $finish(0);
   endrule
endmodule
