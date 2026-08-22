import FIFOF::*;

(* synthesize *)
module sysListSelectTest(Empty);
  Reg#(UInt#(8)) cycle <- mkReg(0);

  rule tick;
    cycle <= cycle + 1;
    if (cycle == 1) $finish;
  endrule

  List#(UInt#(8)) l = Cons(10, Cons(20, Cons(30, Cons(40, Nil))));

  rule show (cycle == 0);
    $display("select 0: %0d", primListSelect(l, 0));
    $display("select 1: %0d", primListSelect(l, 1));
    $display("select 2: %0d", primListSelect(l, 2));
    $display("select 3: %0d", primListSelect(l, 3));
    $display("length: %0d", primListLength(l));
  endrule

endmodule
