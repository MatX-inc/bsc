
import "BVI" VMod =
module mkMod (Reg#(Bool));

   method _write (D_IN) enable (EN);
   method Q_OUT _read;

   schedule _write SBR _write;
   schedule _read CF _read;
   // duplicates, which disagree (SBR is not the reverse of SB)
   schedule _read SB _write;
   schedule _write SBR _read;
endmodule

