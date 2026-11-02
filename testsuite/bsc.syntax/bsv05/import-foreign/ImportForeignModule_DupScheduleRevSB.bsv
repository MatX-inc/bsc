
import "BVI" VMod =
module mkMod (Reg#(Bool));

   method _write (D_IN) enable (EN);
   method Q_OUT _read;

   schedule _write SBR _write;
   schedule _read CF _read;
   // duplicates, which disagree because SB is not symmetric
   schedule _read SB _write;
   schedule _write SB _read;
endmodule
