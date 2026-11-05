TOP:=$(shell pwd)/../..

# -----
# System tools

INSTALL ?= install
CABAL ?= cabal

RM = rm -f

FIND = find

# -----
# Options

# Set this to 1 if the source files are not in Git or the Git tool
# is not available.  The variable is used by "update-build-version.sh"
# which will not try running Git if the value is 1.
#
NOGIT ?= 0
export NOGIT

# With NOGIT=1, the build version will be update to a null value.
# If you want to pre-create the file and avoid overwriting it,
# then set this variable to 1.
#
NOUPDATEBUILDVERSION ?= 0
export NOUPDATEBUILDVERSION

# -----
# Paths and environment

# PREFIX is where things are installed
PREFIX?=$(TOP)/inst
BINDIR=$(PREFIX)/bin

# Where package information is installed
LICDIR=$(PREFIX)/LICENSES

# -----
# GHC

GHC ?= ghc
# Parallelism for `cabal build -j`. Default: processor count, capped at 16.
GHCJOBS ?= $(shell n=`nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 1`; if [ "$$n" -gt 16 ]; then echo 16; else echo "$$n"; fi)

# RTS options for compiling the bsc package: 3 GB base + 1 GB per job.
GHCMAXHEAP ?= $(shell expr 3 + $(GHCJOBS))G
GHCRTSFLAGS ?= +RTS -M$(GHCMAXHEAP) -A256m -RTS

# Build variant (PROF, DEBUG, or NOOPT) selects cabal.project.<variant>.
ifneq ($(BSC_BUILD),)
CABAL_PROJECT = cabal.project.$(shell echo $(BSC_BUILD) | tr A-Z a-z)
ifeq ($(wildcard $(TOP)/$(CABAL_PROJECT)),)
$(error BSC_BUILD=$(BSC_BUILD): no $(CABAL_PROJECT) (INSTALL.md))
endif
endif

CABAL_FLAGS = $(if $(CABAL_PROJECT),--project-file=$(CABAL_PROJECT)) --with-compiler=$(GHC) $(if $(strip $(GHCRTSFLAGS)),--ghc-options='$(GHCRTSFLAGS)')

# -----
# GHC package info

# If BSC_BUILD_PKGINFO is non-empty, generate LICENSES/LICENSE.ghc_pkgs.
ifeq ($(BSC_BUILD_PKGINFO),)
PKGINFO=
else
PKGINFO=$(LICDIR)/LICENSE.ghc_pkgs

# The ghc-pkg beside $(GHC), named like it (ghc-pkg-9.4.7 for ghc-9.4.7);
# set GHCPKG when the names differ.
GHCPKG ?= $(if $(findstring /,$(firstword $(GHC))),$(dir $(firstword $(GHC))))ghc-pkg$(patsubst ghc%,%,$(notdir $(firstword $(GHC))))

.PHONY: $(PKGINFO)
$(PKGINFO):
	mkdir -p -m 755 $(LICDIR)
	cd $(TOP) && GHCPKG="$(GHCPKG)" $(CABAL) exec -v0 $(CABAL_FLAGS) -- bash src/comp/make-ghc-pkg-info.sh --deps-of bsc > $(PKGINFO)
endif

# -----
# Build and install targets

CABAL_EXES = $(shell sed -n 's/^executable[[:space:]]*\([^[:space:]]*\).*/\1/p' $(TOP)/bsc.cabal)
INSTALLEXES = $(BSCEXES) $(TCLEXES)
EXTRAEXES = $(filter-out $(INSTALLEXES),$(CABAL_EXES))

# Point the linker at the vendored solvers installed in $(PREFIX)/lib/SAT.
CABAL_ENV = LIBRARY_PATH="$(abspath $(PREFIX))/lib/SAT$${LIBRARY_PATH:+:$$LIBRARY_PATH}"

BSCEXES = bsc
TCLEXES = bluetcl
UTILEXES = bsc2bsv bsv2bsc dumpbo dumpba vcdcheck
SHOWRULESEXES = showrules

.PHONY: version
version:
	./update-build-version.sh --module .

# Serialize this Make wrapper; Cabal parallelizes internally via -j$(GHCJOBS).
.NOTPARALLEL:

.DEFAULT_GOAL := all
.PHONY: all
all: $(CABAL_EXES)

.PHONY: $(CABAL_EXES)
$(CABAL_EXES): version
	cd $(TOP) && $(CABAL_ENV) $(CABAL) build -j$(GHCJOBS) $(CABAL_FLAGS) exe:$@
	ln -sf "$$(cd $(TOP) && $(CABAL) list-bin $(CABAL_FLAGS) bsc:exe:$@)" $@

.PHONY: install install-extra
install: CABAL_INSTALL = $(INSTALLEXES)
install: INSTALL_PKGINFO = $(PKGINFO)
install-extra: CABAL_INSTALL = $(EXTRAEXES)
install install-extra: version
	mkdir -p -m 755 $(BINDIR)
	cd $(TOP) && $(CABAL_ENV) $(CABAL) build -j$(GHCJOBS) $(CABAL_FLAGS) $(addprefix exe:,$(CABAL_INSTALL))
	@set -e; for exe in $(CABAL_INSTALL); do \
	  bin=$$(cd $(TOP) && $(CABAL) list-bin $(CABAL_FLAGS) bsc:exe:$$exe); \
	  echo "$(INSTALL) -m 755 $$bin $(BINDIR)/$$exe"; \
	  $(INSTALL) -m 755 "$$bin" $(BINDIR)/$$exe; \
	done
	$(if $(INSTALL_PKGINFO),$(MAKE) $(INSTALL_PKGINFO))

.PHONY: install-bsc install-bluetcl install-utils install-showrules
install-bsc: ; $(MAKE) install CABAL_INSTALL="$(BSCEXES)" INSTALL_PKGINFO=
install-bluetcl: ; $(MAKE) install CABAL_INSTALL="$(TCLEXES)" INSTALL_PKGINFO=
install-utils: ; $(MAKE) install CABAL_INSTALL="$(UTILEXES)" INSTALL_PKGINFO=
install-showrules: ; $(MAKE) install CABAL_INSTALL="$(SHOWRULESEXES)" INSTALL_PKGINFO=

# ----
# Other targets

tags:   *hs */*hs
	$(FIND) . ../Parsec ../vendor/stp/HaskellIfc ../vendor/yices/v2.6/HaskellIfc ../vendor/htcl \
		-type f -name \*hs | xargs hasktags
	mv tags tags.tmpfile
	sort tags.tmpfile > tags
	$(RM) tags.tmpfile

# -----
# Clean targets

.PHONY: clean
clean:
	$(RM) -rf $(CABAL_EXES) $(TOP)/dist-newstyle $(TOP)/.ghc.environment.*

.PHONY: full_clean
full_clean: clean
	$(RM) -f BuildVersion.hs Warmup.hs
	$(RM) tags TAGS

# -----
