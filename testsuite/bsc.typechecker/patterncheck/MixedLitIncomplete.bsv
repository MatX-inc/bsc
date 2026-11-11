function Bool maskPartial(Bit#(4) x);
   return (case (x) matches
              4'b1?01: True;
           endcase);
endfunction

typedef union tagged {
   Bit#(4) Payload;
   void Empty;
   void Other;
} MaskedUnion deriving (Bits, Eq);

// The unknown part of Payload must not abandon analysis of the outer tag.
function Bool maskDoesNotSuppressMissingTag(MaskedUnion x);
   return (case (x) matches
              tagged Payload 4'b1???: True;
              tagged Empty: False;
           endcase);
endfunction
