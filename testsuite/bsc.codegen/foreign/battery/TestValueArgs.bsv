import ValueImports::*;

// Computed arguments of an import with a polymorphic argument type.
// Bluesim passes such an argument by address, so the generated C++
// must hold each of these in a variable rather than inline the
// expression into the call.

(* synthesize *)
module mkTestValueArgs ();
   Reg#(Bool) b <- mkReg(True);
   Reg#(Bit#(64)) r <- mkReg(64'h0000000100000002);

   rule disp (b);
      Bit#(48) s = truncate(r);
      $display(" andN = %h", andN(r | 64'h1, r));
      $display(" andN = %h", andN(zeroExtend(r[31:0]), r));
      $display(" andN = %h", andN(s | 48'h1, s));
      b <= False;
   endrule

   rule fin (!b);
      $finish(0);
   endrule
endmodule
