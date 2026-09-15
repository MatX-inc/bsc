# Shadow modules

GHC takes the first match on its search path, and this directory sits
ahead of `../../comp` there.  A module placed here is the one `trs-bir`
compiles against, in place of the compiler's module of the same name —
the way the exporter behaves differently from bsc without a compiler
file being edited.

This directory is empty; `trs-bir` compiles against the compiler's
modules throughout.  What belongs here is anything the exporter must
derive for itself, the dynamic-schedule information first: a stock `.ba`
does not carry it.
