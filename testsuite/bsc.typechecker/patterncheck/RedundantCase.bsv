typedef union tagged {
   void Red;
   Bit#(8) Green;
   Bool Blue;
} Color deriving (Bits, Eq);

// the second Red arm can never match
function Bit#(8) dupRed(Color c);
   return (case (c) matches
              tagged Red: 0;
              tagged Red: 1;
              default: 2;
           endcase);
endfunction

// the default arm can never match, all constructors are covered
function Bit#(8) deadDefault(Color c);
   return (case (c) matches
              tagged Red: 0;
              tagged Green .g: g;
              tagged Blue .b: 1;
              default: 2;
           endcase);
endfunction

// an arm after a wildcard can never match
function Bit#(8) afterWild(Color c);
   return (case (c) matches
              .*: 0;
              tagged Red: 1;
           endcase);
endfunction
