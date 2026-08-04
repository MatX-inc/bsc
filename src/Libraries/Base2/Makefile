PWD:=$(shell pwd)
TOP:=$(PWD)/../../..

# Requires that TOP be set
# Sets BUILDDIR, and BSC and BSCFLAGS if not set
include ../common.mk

SRCS = $(wildcard *.bs) $(wildcard *.bsv)

# Only .bo files are generated (no modules are elaborated in this directory)
OBJS = $(addprefix $(BUILDDIR)/,$(addsuffix .bo,$(basename $(SRCS))))
BCS  = $(addprefix $(BUILDDIR)/,$(addsuffix .bc,$(basename $(SRCS))))

.PHONY: build
build: $(BUILDDIR) $(OBJS) $(BCS)

$(BUILDDIR):
	mkdir -p $@

# .bs.bo:
$(BUILDDIR)/%.bo: %.bs
	${BSC} ${BSCFLAGS} $<

# .bsv.bo:
$(BUILDDIR)/%.bo: %.bsv
	${BSC} ${BSCFLAGS} $<

# .bs.bc / .bsv.bc: signature-only, for downstream -check-only compiles
$(BUILDDIR)/%.bc: %.bs
	${BSC} ${BSCFLAGS} -check-only $<

$(BUILDDIR)/%.bc: %.bsv
	${BSC} ${BSCFLAGS} -check-only $<

.PHONY: clean full_clean
clean full_clean:
	@rm -f *.bi *.bo *.ba vpi_wrapper* *.info *~ depends_bc.mk

# remake the dependency file, requires bluetcl and more to boot strap
.PHONY: depends
depends:
	bluetcl -exec makedepend -bdir "$(BUILDDIR)" "$(SRCS)" | sed -e 's|$(BUILDDIR)|$$(BUILDDIR)|g' > depends.mk

sinclude depends.mk

# A .bc needs the same packages its .bo does, so depends.mk stays the one
# source of truth and the .bc graph is derived from it.
depends_bc.mk: depends.mk
	sed -e 's|\.bo\b|.bc|g' $< > $@

sinclude depends_bc.mk
