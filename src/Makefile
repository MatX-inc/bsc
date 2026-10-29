PWD := $(shell pwd)
TOP := $(PWD)/..

include $(TOP)/platform.mk

PREFIX?=$(TOP)/inst
override PREFIX := $(abspath $(PREFIX))

GHC ?= ghc
CABAL ?= cabal

ifndef NO_DEPS_CHECKS
CC_TOOLS=$(CC) $(CXX) $(LD)
# cabal-install fetches the Haskell libraries (bsc.cabal); ghc-pkg, for the
# package information, is found beside GHC (comp/Makefile, GHCPKG).
BSC_TOOLS=$(GHC) $(CABAL)
YICES_TOOLS=gperf autoconf
STP_TOOLS=flex bison

TOOLS=$(CC_TOOLS) \
      $(BSC_TOOLS) \
      $(YICES_TOOLS) \
      $(STP_TOOLS) \

SUBMODS=vendor/yices/v2.6/yices2

# Check all the necessary build tools are present
pathsearch=$(if $(findstring /,$(1)),$(wildcard $(1)),$(firstword $(wildcard $(addsuffix /$(1),$(subst :, ,$(PATH))))))
checktool=$(if $(call pathsearch,$(1)),,$(error Tool $(1) could not be found))
$(foreach tool,$(TOOLS),$(call checktool,$(tool)))

# The cabal-install the hooks need, checked before the solvers build.
CABALVERSION := $(shell $(CABAL) --numeric-version 2>/dev/null)
CABALMAJOR := $(word 1,$(subst ., ,$(CABALVERSION)))
CABALMINOR := $(word 2,$(subst ., ,$(CABALVERSION)))
ifneq ($(shell expr $(CABALMAJOR) \> 3 \| $(CABALMAJOR) = 3 \& $(CABALMINOR) \>= 16),1)
$(error cabal-install 3.16 or newer is required, found $(CABALVERSION) (INSTALL.md))
endif

# Check all submodules are initialized
submodexists=$(shell git submodule status $(1) 2> /dev/null )
in_git_repo:=$(foreach submod,$(SUBMODS),$(call submodexists,$(submod)))
ifneq ($(in_git_repo),)
define SUBMOD_MSG
Submodule $(1) missing. Initialize with
    git submodule update --init --recursive
endef
checksubmod=$(if $(wildcard $(1)/.git),,$(error $(SUBMOD_MSG)))
else
define SUBMOD_MSG
Submodule $(1) missing.
    This archive was exported from Git without the source files for
    submodules. You will need to acquire those files or, preferably,
    build instead from a clone of the BSC Git repository
endef
checksubmod=$(if $(wildcard $(1)/*),,$(error $(SUBMOD_MSG)))
endif
$(foreach submod,$(SUBMODS),$(call checksubmod,$(submod)))
endif # NO_DEPS_CHECKS

.PHONY: all
all: install

# The solvers alone, for a cabal build by hand (doc/cabal.md section 3).
.PHONY: install-solvers
install-solvers:
	$(MAKE)  -C vendor/stp   PREFIX=$(PREFIX)  install
	$(MAKE)  -C vendor/yices PREFIX=$(PREFIX)  install

.PHONY: install clean full_clean
install clean full_clean:
	$(MAKE)  -C vendor/stp   PREFIX=$(PREFIX)  $@
	$(MAKE)  -C vendor/yices PREFIX=$(PREFIX)  $@
	# comp is one cabal build, which parallelizes itself (GHCJOBS)
	$(MAKE)  -C comp -j1   PREFIX=$(PREFIX)  $@
	$(MAKE)  -C Libraries  PREFIX=$(PREFIX)  $@
	$(MAKE)  -C exec       PREFIX=$(PREFIX)  $@
	$(MAKE)  -C VPI        PREFIX=$(PREFIX)  $@
	$(MAKE)  -C Verilog    PREFIX=$(PREFIX)  $@
	$(MAKE)  -C Verilog.Quartus  PREFIX=$(PREFIX)  $@
	$(MAKE)  -C Verilog.Vivado   PREFIX=$(PREFIX)  $@
	$(MAKE)  -C bluetcl    PREFIX=$(PREFIX)  $@
	$(MAKE)  -C bluesim    PREFIX=$(PREFIX)  $@
	$(MAKE)  -C Verilator  PREFIX=$(PREFIX)  $@

