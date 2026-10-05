PWD:=$(shell pwd)
TOP:=$(PWD)/../..

PREFIX?=$(TOP)/inst
INSTALLDIR = $(PREFIX)/lib/tcllib/bluespec
HELPERDIR = $(TOP)/util/bluetcl-scripts

TCLSH = $(shell $(TOP)/platform.sh tclsh)

FILES = \
	tclIndex \
	pkgIndex.tcl \
	bluespec.tcl \

# tcl files which need each proc listed in tclIndex
# utils.tcl is really a package, but some initization code require it
TCLFILES = \
	utils.tcl \

# tcl packages
PACKAGES = \
        BluesimP.tcl \
        $(HELPERDIR)/InstSynth.tcl \
        $(HELPERDIR)/portUtil.tcl \
        $(HELPERDIR)/types.tcl \

# top level user scripts
EXEFILES = \
	bluesim.tcl \
	makedepend.tcl \
	$(HELPERDIR)/expandPorts.tcl \

.PHONY: all
all: tclIndex pkgIndex.tcl

tclIndex: $(TCLFILES) $(PACKAGES) tclIndex.tcl Makefile
	${TCLSH} ./tclIndex.tcl "$(TCLFILES)" "$(PACKAGES)"

pkgIndex.tcl: tclIndex
	test -f $@ || ${TCLSH} ./tclIndex.tcl "$(TCLFILES)" "$(PACKAGES)"

.PHONY: install
install: tclIndex pkgIndex.tcl
	install -d -m755 $(INSTALLDIR)
	install -m644 $(FILES) $(PACKAGES) $(TCLFILES) $(INSTALLDIR)
	install -m755 $(EXEFILES) $(INSTALLDIR)

.PHONY: clean
clean:
	rm -f tclIndex pkg_mkIndex.tcl pkgIndex.tcl

.PHONY: full_clean
full_clean: clean

