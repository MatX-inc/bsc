#!/bin/sh

# CI calls this from the repository root; it can also be run in testsuite.
# Hidden suite directories contain archived evidence, not current test output.
find . \( -path './.*' -o -path './testsuite/.*' \) -prune -o \( \
    -name bsc.log -o \
    -name bsc.sum -o \
    -name '*.diff-out' -o \
    -name '*.bsc-out' -o \
    -name '*.bsc-ccomp-out' -o \
    -name '*.bsc-vcomp-out' -o \
    -name '*.cxx-comp-out' -o \
    -name '*.bsc-sched-out' \) -print |
    tar zcf logs.tar.gz -T -

