import Vector::*;

typedef enum { Open, ReadFour, ReadTwo, Times, Display, Done } State
   deriving (Eq, Bits);

typedef Int#(32) DataT;

(* synthesize *)
module sysRepeatedTask_LetBound() ;

   Reg#(State) s <- mkReg(Open);

   Reg#(File) fr1 <- mkRegU;
   Reg#(File) fr2 <- mkRegU;

   Reg#(Vector#(4, DataT)) rgs4 <- mkReg(replicate(0));
   Reg#(Vector#(2, DataT)) rgs2 <- mkReg(replicate(0));

   Reg#(Bit#(32)) t1 <- mkReg(0);
   Reg#(Bit#(32)) t2 <- mkReg(0);

   function ActionValue#(Vector#(2, a)) twice(ActionValue#(a) av);
      actionvalue
         let x <- av;
         let y <- av;
         return cons(x, cons(y, nil));
      endactionvalue
   endfunction

   // Each execution of one ActionValue (bound by a let, or passed to a
   // function) is its own call of the system task.

   rule r_open ( s == Open ) ;
      ActionValue#(File) open_data = $fopen("data.txt", "r");
      Vector#(2, File) fs <- twice(open_data);
      fr1 <= fs[0];
      fr2 <= fs[1];
      s <= ReadFour ;
   endrule

   rule r_read_four ( s == ReadFour ) ;
      ActionValue#(DataT) getc = $fgetc(fr1);
      Vector#(4, DataT) v <- replicateM(getc);
      rgs4 <= v;
      s <= ReadTwo ;
   endrule

   rule r_read_two ( s == ReadTwo ) ;
      Vector#(2, DataT) v <- twice($fgetc(fr2));
      rgs2 <= v;
      s <= Times ;
   endrule

   // the two results are equal, but each is the result of its own call
   rule r_times ( s == Times ) ;
      ActionValue#(Bit#(32)) now = $stime;
      let a <- now;
      let b <- now;
      t1 <= a;
      t2 <= b;
      s <= Display ;
   endrule

   rule r_display ( s == Display ) ;
      $display("same file = %b", fr1 == fr2);
      for (Integer i = 0; i < 4; i = i + 1)
         $display("rgs4[%0d] = %h", i, rgs4[i]);
      for (Integer i = 0; i < 2; i = i + 1)
         $display("rgs2[%0d] = %h", i, rgs2[i]);
      s <= Done ;
   endrule

   rule r_done ( s == Done ) ;
      $finish(0) ;
   endrule

endmodule
