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

# -----
# Paths and environment

# PREFIX is where things are installed
PREFIX?=$(TOP)/inst
BINDIR=$(PREFIX)/bin

# Where package information is installed
LICDIR=$(PREFIX)/LICENSES

# The Haskell sources beyond this directory (tags)
PARSEC_HS = ../Parsec
STP_HS = ../vendor/stp/include_hs
YICES_HS = ../vendor/yices/include_hs
HTCL_HS = ../vendor/htcl

# -----
# GHC

GHC ?= ghc
# Parallelism of the cabal build (`cabal build -jN`). From GHC 9.12 the
# project's job semaphore makes N the modules compiling at once; below, each
# package compiles serially and N bounds the packages. Default: the processor
# count, capped at 16 (measured: nothing past 12).
GHCJOBS ?= $(shell n=`nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 1`; if [ "$$n" -gt 16 ]; then echo 16; else echo "$$n"; fi)

GHCVERSION=$(shell $(GHC) --numeric-version 2>/dev/null)
# No version printed: GHC= names nothing on PATH, or there is no GHC.
ifeq ($(GHCVERSION),)
$(error GHC not found: '$(GHC) --version' printed no version (INSTALL.md))
endif
$(info Building with GHC $(GHCVERSION))
# bsc.cabal's GHC floor, checked here rather than reported by cabal's
# solver; a series newer than its upper bounds warns.
GHCMAJOR=$(shell echo $(GHCVERSION) | cut -d. -f1)
GHCMINOR=$(shell echo $(GHCVERSION) | cut -d. -f2)
GHCPATCH=$(shell echo $(GHCVERSION) | cut -d. -f3)
ifeq ($(shell expr $(GHCMAJOR) \< 9 \| $(GHCMAJOR) = 9 \& \( $(GHCMINOR) \< 4 \| $(GHCMINOR) = 4 \& $(GHCPATCH) \< 7 \)),1)
$(error Unsupported GHC $(GHCVERSION): 9.4.7 or newer is required (INSTALL.md))
endif
ifeq ($(shell expr $(GHCMAJOR) \> 9 \| $(GHCMAJOR) = 9 \& $(GHCMINOR) \> 14),1)
$(warning GHC $(GHCVERSION) is newer than the series bsc.cabal's upper bounds admit (through 9.14): raise them, or allow-newer in cabal.project.local (INSTALL.md))
endif

# RTS options for every GHC that compiles the package (the dependencies,
# built into the store, take GHC's defaults): a heap cap of 3 GB plus 1 GB
# per job and a 256 MB allocation area, the measured plateau. CI sets its own.
GHCMAXHEAP ?= $(shell expr 3 + $(GHCJOBS))G
GHCRTSFLAGS ?= +RTS -M$(GHCMAXHEAP) -A256m -RTS

# The build variant, a project file over cabal.project: PROF, DEBUG or NOOPT
# selects cabal.project.prof, .debug or .noopt (INSTALL.md).
ifneq ($(BSC_BUILD),)
CABAL_PROJECT = cabal.project.$(shell echo $(BSC_BUILD) | tr A-Z a-z)
ifeq ($(wildcard $(TOP)/$(CABAL_PROJECT)),)
$(error BSC_BUILD=$(BSC_BUILD): no $(CABAL_PROJECT) (INSTALL.md))
endif
endif

# Beyond the project's settings: the GHC, the variant, the RTS options.
# list-bin takes the same, since the build directory follows them.
CABAL_COMPILER_OPTIONS = --with-compiler=$(GHC)
CABAL_FLAGS = $(if $(CABAL_PROJECT),--project-file=$(CABAL_PROJECT)) $(CABAL_COMPILER_OPTIONS) $(if $(strip $(GHCRTSFLAGS)),--ghc-options='$(GHCRTSFLAGS)')

# -----
# GHC package info

# If BSC_BUILD_PKGINFO is non-empty,
# generate and install a LICENSES/LICENSE.ghc_pkgs file

ifeq ($(BSC_BUILD_PKGINFO),)
PKGINFO=
else
PKGINFO=$(LICDIR)/LICENSE.ghc_pkgs

# The selected GHC's ghc-pkg, for make-ghc-pkg-info.sh: ghc-9.4.7 has
# ghc-pkg-9.4.7 beside it; a wrapper may set GHCPKG.
GHCPKG ?= $(if $(findstring /,$(GHC)),$(dir $(GHC)))ghc-pkg$(patsubst ghc%,%,$(notdir $(GHC)))

# The package databases the programs were linked from: the in-place one,
# cabal's store and GHC's own (the trailing colon).
CABAL_COMPILER_STORE := $(shell cd $(TOP) && $(CABAL) path $(CABAL_COMPILER_OPTIONS) --compiler-info | sed -n 's/^compiler-store-path: //p')
ifeq ($(CABAL_COMPILER_STORE),)
$(error cabal path reports no compiler-store-path for $(GHC))
endif
CABAL_PKG_PATH = $(TOP)/dist-newstyle/packagedb/ghc-$(GHCVERSION):$(CABAL_COMPILER_STORE)/package.db:

.PHONY: $(PKGINFO)
$(PKGINFO):
	mkdir -p -m 755 $(LICDIR)
	GHCPKG="$(GHCPKG)" GHC_PACKAGE_PATH="$(CABAL_PKG_PATH)" ./make-ghc-pkg-info.sh --deps-of bsc > $(PKGINFO)
endif

# -----
# Install targets

# One `cabal build all` at the root compiles the library once and links every
# program against it (doc/cabal.md section 1). `install` copies bsc and
# bluetcl into $(BINDIR), `install-extra` every other executable of bsc.cabal.
CABAL_EXES = $(shell sed -n 's/^executable[[:space:]]*\([^[:space:]]*\).*/\1/p' $(TOP)/bsc.cabal)
INSTALLEXES = $(BSCEXES) $(TCLEXES)
EXTRAEXES = $(filter-out $(INSTALLEXES),$(CABAL_EXES))

# The install stamp, HEAD and the version name, a declared source of the
# package: BuildVersion's rule runs only when cabal-install rebuilds the
# package, so without it a commit touching no source would ship the previous
# version (doc/cabal.md section 4).
CABAL_HEAD = $(TOP)/build/cabal-head

# The solvers link by name from no directory. LIBRARY_PATH names the vendored
# copies src/Makefile installed; without it the linker takes the system's.
CABAL_ENV = LIBRARY_PATH="$(abspath $(PREFIX))/lib/SAT$${LIBRARY_PATH:+:$$LIBRARY_PATH}"

BSCEXES = bsc
TCLEXES = bluetcl
UTILEXES = bsc2bsv bsv2bsc dumpbo dumpba vcdcheck
SHOWRULESEXES = showrules

# Generated modules an older checkout's make build left here; the hooks
# refuse to build over them (doc/cabal.md section 4).
.PHONY: no-stale-modules
no-stale-modules:
	$(RM) -f BuildVersion.hs Warmup.hs

# The programs, each linked beside this Makefile as the make build left them;
# the link resolves the solvers through the build tree's lib/SAT, which a
# copy here would not. A build here by hand needs `make install-solvers` first.
.PHONY: all
all: $(CABAL_EXES)

.PHONY: $(CABAL_EXES)
$(CABAL_EXES): no-stale-modules
	cd $(TOP) && $(CABAL_ENV) $(CABAL) build -j$(GHCJOBS) $(CABAL_FLAGS) exe:$@
	ln -sf "$$(cd $(TOP) && $(CABAL) list-bin $(CABAL_FLAGS) bsc:exe:$@)" $@

.PHONY: install install-extra
install: CABAL_INSTALL = $(INSTALLEXES)
install-extra: CABAL_INSTALL = $(EXTRAEXES)
install install-extra: no-stale-modules
	mkdir -p -m 755 $(TOP)/build $(BINDIR)
	./update-build-version.sh --stamp $(CABAL_HEAD)
	cd $(TOP) && $(CABAL_ENV) $(CABAL) build -j$(GHCJOBS) $(CABAL_FLAGS) all
	@set -e; for exe in $(CABAL_INSTALL); do \
	  bin=$$(cd $(TOP) && $(CABAL) list-bin $(CABAL_FLAGS) bsc:exe:$$exe); \
	  echo "$(INSTALL) -m 755 $$bin $(BINDIR)/$$exe"; \
	  $(INSTALL) -m 755 "$$bin" $(BINDIR)/$$exe; \
	done
ifneq ($(PKGINFO),)
	$(MAKE) $(PKGINFO)
endif

# The make build's group installs, over the one build.
.PHONY: install-bsc install-bluetcl install-utils install-showrules
install-bsc: ; $(MAKE) install CABAL_INSTALL="$(BSCEXES)"
install-bluetcl: ; $(MAKE) install CABAL_INSTALL="$(TCLEXES)"
install-utils: ; $(MAKE) install CABAL_INSTALL="$(UTILEXES)"
install-showrules: ; $(MAKE) install CABAL_INSTALL="$(SHOWRULESEXES)"

# ----
# Other targets

tags:   *hs */*hs
	$(FIND) . $(PARSEC_HS) $(STP_HS) $(YICES_HS) $(HTCL_HS) \
		-type f -name \*hs | xargs hasktags
	mv tags tags.tmpfile
	sort tags.tmpfile > tags
	$(RM) tags.tmpfile

# -----
# Clean targets

# The environment file names dist-newstyle's package database.
.PHONY: clean
clean:
	$(RM) -rf $(CABAL_EXES) $(TOP)/dist-newstyle $(CABAL_HEAD) $(TOP)/.ghc.environment.*

.PHONY: full_clean
full_clean: clean no-stale-modules
	$(RM) tags TAGS

# -----
