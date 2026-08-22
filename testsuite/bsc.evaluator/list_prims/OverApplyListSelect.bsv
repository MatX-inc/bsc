import List::*;

// Selecting a function out of a list and applying it: the trailing
// argument must be forwarded through primListSelect.

(* synthesize *)
module sysOverApplyListSelect(Empty);

  Reg#(Bit#(8)) cycle <- mkReg(0);

  function Bit#(8) addN(Bit#(8) n, Bit#(8) x) = x + n;

  List#(function Bit#(8) f(Bit#(8) x)) fns = Cons(addN(1), Cons(addN(2), Nil));

  rule show;
    $display("O c=%0d a=%0h b=%0h", cycle, (fns[0])(8'h10), (fns[1])(8'h10));
  endrule

  rule tick;
    cycle <= cycle + 1;
    if (cycle == 4) $finish(0);
  endrule

endmodule
