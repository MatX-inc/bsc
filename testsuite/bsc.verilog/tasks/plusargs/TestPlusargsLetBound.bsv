(* synthesize *)
module sysTestPlusargsLetBound ();

   Reg#(Bool) b <- mkReg(True);

   // each execution of the let-bound ActionValue is its own call of
   // $test$plusargs, with its own result
   rule disp_rule (b);
      ActionValue#(Bool) he = $test$plusargs("He");
      Bool v1 <- he;
      Bool v2 <- he;
      $display("v1 = %h", v1);
      $display("v2 = %h", v2);
      b <= False;
   endrule

   rule end_rule (!b);
      $finish(0);
   endrule

endmodule
