function Bool zeroWidthIntIsComplete(Int#(0) x);
   case (x) matches
      0: return True;
   endcase
endfunction
