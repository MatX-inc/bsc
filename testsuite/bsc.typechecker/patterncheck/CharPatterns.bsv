function Bool incompleteChar(Char c);
   case (c) matches
      "a": return True;
   endcase
endfunction

function Bool redundantChar(Char c);
   case (c) matches
      "b": return False;
      "b": return True;
      default: return True;
   endcase
endfunction
