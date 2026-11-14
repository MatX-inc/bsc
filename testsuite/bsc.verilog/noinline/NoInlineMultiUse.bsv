// One call of a noinline function is one instance of the function's
// module.  The result of the call can define several signals: the
// argument of several register writes or submodule methods, the enable
// or the condition of several method calls, the result of several
// value methods.  All of them must be driven by the one instance.
// They must not each get a copy of the call: the copies would be
// instances with one name, which is not legal Verilog.

(* noinline *)
function Bit#(8) multiUseEncode(Bit#(8) x);
   return x ^ (x << 1);
endfunction

(* noinline *)
function Bool multiUsePick(Bit#(8) x);
   return x[0] == 1;
endfunction

(* noinline *)
function Bit#(8) multiUseFold(Bit#(8) x);
   return {x[3:0], x[7:4]};
endfunction

interface NoInlineMultiUse_Sub;
   method Action poke();
   method Action put(Bit#(8) x);
   method Bit#(4) count();
   method Bit#(8) get1();
   method Bit#(8) get2();
endinterface

(* synthesize *)
module mkNoInlineMultiUse_Sub(NoInlineMultiUse_Sub);
   Reg#(Bit#(4)) c <- mkReg(0);
   Reg#(Bit#(8)) d <- mkReg(0);

   // one call, the result of two value methods
   let f = multiUseFold(d);

   method Action poke();
      c <= c + 1;
   endmethod

   method Action put(Bit#(8) x);
      d <= x;
   endmethod

   method count = c;
   method get1 = f;
   method get2 = f;
endmodule

(* synthesize *)
module sysNoInlineMultiUse();
   Reg#(Bit#(8)) a  <- mkReg(1);
   Reg#(Bit#(8)) r1 <- mkReg(0);
   Reg#(Bit#(8)) r2 <- mkReg(0);
   Reg#(Bit#(8)) r3 <- mkReg(0);

   NoInlineMultiUse_Sub s1 <- mkNoInlineMultiUse_Sub;
   NoInlineMultiUse_Sub s2 <- mkNoInlineMultiUse_Sub;
   NoInlineMultiUse_Sub s3 <- mkNoInlineMultiUse_Sub;
   NoInlineMultiUse_Sub s4 <- mkNoInlineMultiUse_Sub;

   rule step;
      a <= a + 1;
   endrule

   // one call, the argument of three register writes
   rule write;
      let v = multiUseEncode(a);
      r1 <= v;
      r2 <= v;
      r3 <= v;
   endrule

   // a second call of the same function (so a second instance),
   // the argument of two submodule methods
   rule put;
      let w = multiUseEncode(r1);
      s1.put(w);
      s2.put(w);
   endrule

   // one call, the enable of two submodule methods and nothing else
   rule poke (multiUsePick(a));
      s1.poke;
      s2.poke;
   endrule

   // a second call of the same function (so a second instance),
   // the condition of two method calls
   rule cond;
      let p = multiUsePick(r1);
      if (p) s3.poke;
      if (p) s4.poke;
   endrule

   rule show;
      $display("%h %h %h %h %d %d %d %d %h %h",
               a, r1, r2, r3, s1.count, s2.count, s3.count, s4.count,
               s1.get1, s2.get2);
      if (a == 8) $finish(0);
   endrule
endmodule
