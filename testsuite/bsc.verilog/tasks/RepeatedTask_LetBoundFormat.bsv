typedef enum { SWrite, SFormat, Time, Done } State deriving (Eq, Bits);

(* synthesize *)
module sysRepeatedTask_LetBoundFormat() ;

   Reg#(State) s <- mkReg(SWrite);

   Reg#(Bit#(8)) c <- mkReg(65);

   function ActionValue#(Tuple2#(a, a)) twice(ActionValue#(a) av);
      actionvalue
         let x <- av;
         let y <- av;
         return tuple2(x, y);
      endactionvalue
   endfunction

   // Each execution of a let-bound $swriteAV, $sformatAV or $time is its
   // own call of the task, with its own result.  The two results of each
   // rule are equal, so the .exp also checks the generated Verilog.

   rule r_swrite ( s == SWrite ) ;
      ActionValue#(Bit#(8)) w = $swriteAV("%c", c);
      match {.a, .b} <- twice(w);
      $display("swriteAV %c %c", a, b);
      s <= SFormat ;
   endrule

   rule r_sformat ( s == SFormat ) ;
      ActionValue#(Bit#(16)) f = $sformatAV("%c%c", c, c + 1);
      match {.a, .b} <- twice(f);
      $display("sformatAV %s %s", a, b);
      s <= Time ;
   endrule

   rule r_time ( s == Time ) ;
      ActionValue#(Bit#(64)) now = $time;
      match {.a, .b} <- twice(now);
      $display("same time = %b", a == b);
      s <= Done ;
   endrule

   rule r_done ( s == Done ) ;
      $finish(0) ;
   endrule

endmodule
