# trs-bir: the exporter, built from bsc's own modules.
#
# The sources stay where bsc keeps them and this build reaches them on
# the GHC search path.  Side modules in shadow/ sit ahead of ../../comp
# on that path, so a module this program needs to behave differently is
# a file here rather than an edit to a compiler file.
#
# The module closure is a fraction of bsc's: the elaborator, type
# checker, and code generators all sit downstream of the .ba this
# program reads.

TOP:=$(shell pwd)/../../..
PREFIX?=$(TOP)/inst
BINDIR=$(PREFIX)/bin
BUILDDIR=$(TOP)/build/trs-bir

COMP = ../../comp
VENDOR = ../../vendor

INSTALL ?= install
GHC ?= ghc
GHCJOBS ?= 1

# Side modules come first, so a file placed here wins over the
# compiler's copy of the same module.
SHADOWS = shadow

# Same set bsc builds against; cborg is what SimExportIR encodes with.
PACKAGES = \
	base containers array mtl unix regex-compat bytestring directory \
	process filepath time old-time old-locale split syb integer-gmp \
	text deepseq strict-concurrency cborg

# No SAT solver appears in this closure.  A .ba is already scheduled and
# nothing downstream of it asks a solver anything, so neither STP nor
# Yices is built or linked here -- see AOptCase in the compiler, which
# is the boundary that keeps it that way.
SAT_INCLUDES =
SAT_LIBS =

GHCINCLUDES = \
	-i$(SHADOWS) \
	-i$(COMP) \
	-i$(COMP)/GHC/posix \
	-i$(COMP)/Libs \
	-i../../Parsec \
	$(SAT_INCLUDES)

# This program is built from bsc's own modules, and src/comp builds them
# at -O2; without an -O flag GHC's default is -O0, which more than doubles
# the export (measured 108s -> 46s on a large design, byte-identical .bir)
# and triples its allocation.  ?= so a GHC whose optimiser has a bug can be
# worked around from the command line, the way src/comp does for 9.2.1.
GHCOPTLEVEL ?= -O2

GHCFLAGS = \
	$(GHCOPTLEVEL) \
	-hide-all-packages \
	-fasm \
	-Wall \
	-fno-warn-orphans \
	-fno-warn-name-shadowing \
	-fno-warn-unused-matches \
	$(addprefix -package ,$(PACKAGES)) \
	$(GHCINCLUDES)

GHCMAXHEAP ?= $(shell expr 3 + $(GHCJOBS))G
GHCRTSFLAGS ?= +RTS -M$(GHCMAXHEAP) -A128m -RTS
RTSFLAGS = "-with-rtsopts=-H256m -K10m -i1"

ifeq ($(shell uname -s),Darwin)
GHC += -optl -headerpad_max_install_names
endif

.PHONY: all
all: trs-bir

# Version strings, generated into the compiler directory by the same
# scripts bsc's own build runs.  They are gitignored build products, so
# making them here costs nothing and keeps this build standalone.
$(COMP)/BuildVersion.hs:
	cd $(COMP) && ./update-build-version.sh

$(COMP)/BuildSystem.hs:
	cd $(COMP) && ./update-build-system.sh

.PHONY: trs-bir
trs-bir: $(COMP)/BuildVersion.hs $(COMP)/BuildSystem.hs
	mkdir -p $(BUILDDIR)
	$(GHC) --make Main.hs -o $@ -j$(GHCJOBS) \
		-hidir $(BUILDDIR) -odir $(BUILDDIR) -stubdir $(BUILDDIR) \
		$(GHCFLAGS) $(RTSFLAGS) $(SAT_LIBS) $(GHCRTSFLAGS)

# Installed the way bsc installs: the binary under bin/core, and the
# compiler's own wrapper in bin to name it.  The wrapper supplies
# BLUESPECDIR, which is where the .ba search path ends; with that in the
# environment the binary also runs straight from this directory.
.PHONY: install
install: trs-bir
	$(INSTALL) -m 755 -d $(BINDIR)/core
	$(INSTALL) -m 755 trs-bir $(BINDIR)/core/trs-bir
	$(INSTALL) -m 755 $(COMP)/wrapper.sh $(BINDIR)/trs-bir

.PHONY: clean full_clean
clean full_clean:
	rm -rf $(BUILDDIR) trs-bir
