# This source-level test needs the Cabal package environment. It is separate
# from the installed-compiler DejaGNU suite, which need not include GHC.
.PHONY: check
check:
	bash test.sh

CONFDIR = $(realpath ../..)
include $(CONFDIR)/clean.mk
