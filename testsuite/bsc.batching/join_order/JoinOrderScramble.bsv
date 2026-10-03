package JoinOrderScramble;

// Parsed before JoinOrder in the batch: interns "IsModule" before "Add"
// and "Bits"; JoinOrder writes Add first and never writes IsModule.
module [m] mkJoinOrderScramble(Empty) provisos (IsModule#(m, c), Add#(1, 1, 2), Bits#(Bool, 1));
endmodule

endpackage
