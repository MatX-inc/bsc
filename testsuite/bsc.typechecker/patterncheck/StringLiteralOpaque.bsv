instance StringLiteral#(Bit#(1));
   function Bit#(1) fromString(String s);
      return 0;
   endfunction
endinstance

instance StringLiteral#(Integer);
   function Integer fromString(String s);
      return 0;
   endfunction
endinstance

// These strings have the same runtime value, but fromString is arbitrary user
// code.  The pattern checker must make no exhaustiveness or redundancy claim.
function Bool opaqueFiniteStrings(Bit#(1) x);
   return (case (x) matches
              "a": False;
              "b": True;
              default: False;
           endcase);
endfunction

function Bool opaqueUnboundedStrings(Integer x);
   return (case (x) matches
              "a": False;
              "b": True;
           endcase);
endfunction

typedef union tagged {
   Bit#(1) Payload;
   void Other;
} StringOuter deriving (Bits, Eq);

// An unknown payload conversion must not suppress unrelated outer-tag facts.
function Bool stringDoesNotSuppressMissingTag(StringOuter x);
   return (case (x) matches
              tagged Payload "a": True;
           endcase);
endfunction

function Bool stringDoesNotSuppressDuplicateTag(StringOuter x);
   return (case (x) matches
              tagged Payload "a": True;
              tagged Other: False;
              tagged Other: True;
           endcase);
endfunction
