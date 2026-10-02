package TrieOrderScramble;

// Parsed before TrieOrder in the batch: interns "Module" before
// "ModuleContext", the opposite of TrieOrder's own order (its import of
// ModuleContext comes first; it never writes Module).
typedef Module#(Empty) TrieOrderScrambleAlias;

endpackage
