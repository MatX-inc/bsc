#!/usr/bin/env bash

apt-get update

# ccache is not required to buid bsc, but we use it in build.yml to improve
# the build performance by caching C++ obj files across multiple builds.
# zlib1g-dev is needed to compile libfst (the src/vendor/libfst
# submodule) into the Bluesim kernel library.
# cpio and patchelf are used by `make install-bluehs`.
apt-get install -y \
  ccache \
  autoconf \
  bison \
  build-essential \
  cpio \
  flex \
  git \
  gperf \
  iverilog \
  patchelf \
  tcl-dev \
  zlib1g-dev
