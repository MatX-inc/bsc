PWD:=$(shell pwd)
TOP:=$(PWD)/../../..

# Requires that TOP be set
# Sets BUILDDIR, and BSC and BSCFLAGS if not set
include ../common.mk

SRCS = $(wildcard *.bs) $(wildcard *.bsv)

# Only .bo files are generated (no modules are elaborated in this directory)
OBJS = $(addprefix $(BUILDDIR)/,$(addsuffix .bo,$(basename $(SRCS))))
# and a .bi (the package signature) beside each, for signature-only compiles
BIS  = $(addprefix $(BUILDDIR)/,$(addsuffix .bi,$(basename $(SRCS))))

.PHONY: build
build: $(OBJS) $(BIS)

# Make sure to build the directory before trying to build the objects
# but don't force recompile based on the timestamp of the directory
$(OBJS) $(BIS): | $(BUILDDIR)

$(BUILDDIR):
	mkdir -p $@

# To avoid recursive importing,
# don't auto-include the Prelude when compiling the Prelude
$(BUILDDIR)/Prelude.bo:     Prelude.bs ${BSC}
	${BSC} ${BSCFLAGS} -no-use-prelude Prelude.bs

$(BUILDDIR)/PreludeBSV.bo:  PreludeBSV.bsv ${BSC}
	${BSC} ${BSCFLAGS} -no-use-prelude PreludeBSV.bsv

$(BUILDDIR)/Prelude.bi:     Prelude.bs ${BSC}
	${BSC} ${BSCFLAGS} -signature-only -no-use-prelude Prelude.bs

$(BUILDDIR)/PreludeBSV.bi:  PreludeBSV.bsv ${BSC}
	${BSC} ${BSCFLAGS} -signature-only -no-use-prelude PreludeBSV.bsv

# .bs.bo:
$(BUILDDIR)/%.bo: %.bs
	${BSC} ${BSCFLAGS} $<

# .bsv.bo:
$(BUILDDIR)/%.bo: %.bsv
	${BSC} ${BSCFLAGS} $<

# .bs.bi / .bsv.bi: the package signature, for downstream -signature-only
# and -check-only compiles
$(BUILDDIR)/%.bi: %.bs
	${BSC} ${BSCFLAGS} -signature-only $<

$(BUILDDIR)/%.bi: %.bsv
	${BSC} ${BSCFLAGS} -signature-only $<

.PHONY: clean full_clean
clean full_clean:
	@rm -f *.bi *.bo *.ba vpi_wrapper* *.info *~ depends_bi.mk

# remake the dependency file, requires bluetcl and more to boot strap
.PHONY: depends
depends:
	bluetcl -exec makedepend -bdir "$(BUILDDIR)" "$(SRCS)" | sed -e 's|$(BUILDDIR)|$$(BUILDDIR)|g' > depends.mk

sinclude depends.mk

# A .bi needs the same packages its .bo does, so depends.mk stays the one
# source of truth and the .bi graph is derived from it.
depends_bi.mk: depends.mk
	sed -e 's|\.bo|.bi|g' $< > $@

sinclude depends_bi.mk
