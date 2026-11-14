(* noinline *)
function Bit#(8) multiUseEncode(Bit#(8) x);
   return x ^ (x << 1);
endfunction

(* noinline *)
function Bool multiUsePick(Bit#(8) x);
   return x[0] == 1;
endfunction

interface NoInlineMultiUse_Sub;
   method Action put(Bit#(8) x);
   method Bit#(8) get1();
   method Bit#(8) get2();
endinterface

(* synthesize *)
module mkNoInlineMultiUse_Sub(NoInlineMultiUse_Sub);
   Reg#(Bit#(8)) d <- mkReg(0);
   let enc = multiUseEncode(d);
   method Action put(Bit#(8) x); d <= x; endmethod
   method get1 = enc;
   method get2 = enc;
endmodule

(* synthesize *)
module sysNoInlineMultiUse();
   Reg#(Bit#(8)) a <- mkReg(1);
   Reg#(Bit#(8)) r <- mkReg(0);
   NoInlineMultiUse_Sub s1 <- mkNoInlineMultiUse_Sub;
   NoInlineMultiUse_Sub s2 <- mkNoInlineMultiUse_Sub;

   rule step;
      a <= a + 1;
      let v = multiUseEncode(a);
      r <= v;
      if (multiUsePick(a)) begin
         s1.put(v);
         s2.put(v);
      end
      $display("%h %h %h %h", a, r, s1.get1, s2.get2);
      if (a == 4) $finish(0);
   endrule
endmodule
