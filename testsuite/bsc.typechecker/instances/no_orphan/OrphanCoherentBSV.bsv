package OrphanCoherentBSV;

// "coherent" keeps its meaning next to "orphan", in either order:
// even with -incoherent-instance-matches, the polymorphic uses below
// cannot commit to the catch-all instance (T0030, once per class)

typeclass coherent orphan CoherentFirst#(type a);
   function Bit#(8) coherentFirst(a x);
endtypeclass

instance CoherentFirst#(a);
   function coherentFirst(x) = 1;
endinstance

instance CoherentFirst#(Bool);
   function coherentFirst(x) = 3;
endinstance

function Bit#(8) throughFirst(a x) provisos (Bits#(a, na));
   return coherentFirst(x);
endfunction

typeclass orphan coherent CoherentSecond#(type a);
   function Bit#(8) coherentSecond(a x);
endtypeclass

instance CoherentSecond#(a);
   function coherentSecond(x) = 1;
endinstance

instance CoherentSecond#(Bool);
   function coherentSecond(x) = 3;
endinstance

function Bit#(8) throughSecond(a x) provisos (Bits#(a, na));
   return coherentSecond(x);
endfunction

endpackage
