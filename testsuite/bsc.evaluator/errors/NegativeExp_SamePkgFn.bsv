function Integer myexp(Integer x, Integer y);
   return x ** y;
endfunction

(* synthesize *)
module sysNegativeExp_SamePkgFn();
   rule test;
      $display(myexp(2, -1));
      $finish(0);
   endrule
endmodule
