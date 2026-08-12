# Haskell Language Server

The following instructions are helpful if you wish to develop on the
Haskell sources for bluespec.

Since the Haskell tools are built as an ordinary Cabal package
(`bsc.cabal` at the root of the repository),
[HLS](https://github.com/haskell/haskell-language-server) works out of
the box: open the root of the repository as a workspace in your
favorite IDE or editor with HLS support, and HLS will discover the
project through the Cabal cradle.

The one requirement is that the `cabal` on your PATH is version 3.14
or newer, because the package uses Cabal's `Hooks` build type; the
Cabal library that HLS bundles for GHC 9.6 predates that, so with an
older `cabal` the build works but HLS does not.
