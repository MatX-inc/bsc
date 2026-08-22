{
  outputs = { self, nixpkgs, ... }: {
    devShells = builtins.mapAttrs (
      system: pkgs:
      let
        hs = pkgs.haskell.packages.ghc967;
      in
      {
        default = pkgs.mkShell {
          # Compiler, vendored-solver and Tcl deps all come from the package.
          inputsFrom = [ self.packages.${system}.default ];
          # Dev-only tools: cabal, HLS (matching the package set's GHC), and
          # the testsuite deps.
          nativeBuildInputs = [
            hs.haskell-language-server
            pkgs.cabal-install
            pkgs.dejagnu
            pkgs.iverilog
          ];
        };
      }
    ) nixpkgs.legacyPackages;
    packages = builtins.mapAttrs (
      system: pkgs:
      let
        hs = pkgs.haskell.packages.ghc967;
        yices2 = pkgs.fetchFromGitHub {
          owner = "SRI-CSL";
          repo = "yices2";
          rev = "f705557b7d33d866eb1b47b5471f97189eb31cc4";
          hash = "sha256-qdxh86CkKdm65oHcRgaafTG9GUOoIgTDjeWmRofIpNE=";
        };

      in
      {
        default = hs.mkDerivation {
          pname = "bsc";
          version = "2026.1";
          src = ./.;

          isLibrary = true;
          isExecutable = true;

          # The test suites need the BLUESPECDIR runtime in inst/lib, which
          # is not built by cabal (yet); run them via `cabal test` instead.
          doCheck = false;

          buildTools = [
            pkgs.autoconf
            pkgs.bison
            pkgs.flex
            pkgs.gperf
            pkgs.perl
            pkgs.tcl
            pkgs.which
          ]
          ++ pkgs.lib.optionals pkgs.stdenv.isLinux [
            pkgs.glibc.bin
          ];

          setupHaskellDepends = [
            hs.Cabal_3_16_1_0
            hs.Cabal-hooks
            hs.process_1_6_28_0
          ];

          libraryHaskellDepends = [
            hs.array
            hs.base
            hs.bytestring
            hs.containers
            hs.deepseq
            hs.directory
            hs.filepath
            hs.integer-gmp
            hs.mtl
            hs.old-locale
            hs.old-time
            hs.process_1_6_28_0
            hs.regex-compat
            hs.split
            hs.strict-concurrency
            hs.syb
            hs.text
            hs.time
            hs.unix
          ];
          libraryPkgconfigDepends = [ pkgs.tcl ];
          librarySystemDepends = [
            pkgs.gmp
            pkgs.zlib
          ];

          executableHaskellDepends = [
            hs.base
            hs.bytestring
            hs.containers
            hs.directory
            hs.filepath
            hs.mtl
            hs.old-time
            hs.process_1_6_28_0
            hs.regex-compat
            hs.split
            hs.syb
            hs.time
            hs.unix
          ];

          prePatch = ''
            # Flakes don't include submodules, so we copy in yices.
            cp -r --preserve=timestamps --reflink=auto -- \
              "${yices2}" src/vendor/yices/v2.6/yices2
            chmod -R u+w -- src/vendor/yices/v2.6/yices2
          '';
          postPatch = "patchShebangs .";

          preConfigure = "export NOGIT=1";

          preCompileBuildDriver = ''
            cat > Setup.hs <<EOF
            import Distribution.Simple
            import SetupHooks
            main = defaultMainWithSetupHooks setupHooks
            EOF
          '';

          # Assemble the BLUESPECDIR runtime (lib/Libraries, Bluesim, the
          # Verilog primitives, ...) next to the installed binaries, like
          # `make install-src` does.  The runtime Makefiles pick the compiler
          # up from $(PREFIX)/bin, where the Haskell builder just installed
          # it; NO_DEPS_CHECKS skips src/Makefile's tool probe, which insists
          # on cabal (the builder drives Setup.hs directly).
          postInstall = ''
            make -C src install-runtime PREFIX=$out NO_DEPS_CHECKS=1
          '';
        };
      }
    ) nixpkgs.legacyPackages;
  };
}
