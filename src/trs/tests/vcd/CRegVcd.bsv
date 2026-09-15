// Per-port CReg VCD surface: the reference records Q_OUT_i/EN_i/D_IN_i
// for each port, so a wrong port index shows up as a misplaced write
// even though the chained value the design reads stays correct.
(* synthesize *)
module sysCRegVcd();
   Reg#(UInt#(8)) cyc <- mkReg(0);
   Array#(Reg#(Bit#(8))) cr <- mkCReg(3, 0);
   Reg#(Bit#(8)) sink <- mkReg(0);

   rule tick;
      cyc <= cyc + 1;
      if (cyc == 8) $finish(0);
   endrule

   // each port writes a distinct value, so the per-port D_IN traces
   // are distinguishable
   rule w0 (pack(cyc)[0] == 0);
      cr[0] <= cr[0] + 1;
   endrule

   rule w1 (pack(cyc)[1] == 0);
      cr[1] <= cr[1] + 16;
   endrule

   rule readout;
      sink <= cr[2];
   endrule
endmodule
