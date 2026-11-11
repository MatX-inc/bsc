typedef union tagged {
   void Red;
   Bit#(8) Green;
   Bool Blue;
} Color deriving (Bits, Eq);

function Bit#(8) missingBlue(Color c);
   return (case (c) matches
              tagged Red: 0;
              tagged Green .g: g;
           endcase);
endfunction

function Bit#(8) nestedMissing(Maybe#(Maybe#(Bool)) m);
   return (case (m) matches
              tagged Invalid: 0;
              tagged Valid (tagged Valid True): 1;
           endcase);
endfunction
