# ORACLE-mode witness: run the full gcd stepping session on a dual
# engine set (interp primary + QUIET jit secondary, lockstep-compared
# at every stop).  Output must stay byte-identical to the
# single-engine reference: the secondary's output is suppressed, and
# any divergence prints on stderr and flips the exit to fatal.
source gcd.cmd
