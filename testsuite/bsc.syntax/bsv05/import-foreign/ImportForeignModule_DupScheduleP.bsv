
import "BVI" VMod =
module mkMod (Reg#(Bool));

   method _write (D_IN) enable (EN);
   method Q_OUT _read;

   schedule _write SBR _write;
   schedule _read P _read;
   // duplicates, but the same relationship
   schedule _read P _write;
   schedule _read P _write;
   schedule _write P _read;  // P is symmetric, so swapped is the same
   schedule _read P _read;
endmodule

