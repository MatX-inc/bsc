FROM ubuntu:24.04 AS build
ADD .github/workflows/install_dependencies_ubuntu.sh /build/
RUN DEBIAN_FRONTEND=noninteractive \
    /build/install_dependencies_ubuntu.sh \
    && apt-get update \
    && DEBIAN_FRONTEND=noninteractive apt-get install -y curl \
    && rm -rf /var/lib/apt/lists/*
# GHC and cabal-install come from ghcup (the distro versions are too old);
# build-type: Hooks needs cabal-install 3.14+
ENV GHCUP_INSTALL_BASE_PREFIX=/usr/local
ENV PATH=/usr/local/.ghcup/bin:$PATH
RUN curl --proto '=https' --tlsv1.2 -sSf https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup -o /usr/local/bin/ghcup \
    && chmod +x /usr/local/bin/ghcup \
    && ghcup install ghc 9.6.7 --set \
    && ghcup install cabal 3.16.1.0 --set \
    && cabal update
ADD . /build/
RUN make -C /build -j2 GHCJOBS=2 GHCRTSFLAGS='+RTS -M5G -A128m -RTS' install-src

FROM ubuntu:24.04
RUN apt-get update \
    && DEBIAN_FRONTEND=noninteractive \
       apt-get install -y \
          build-essential tcl iverilog \
    && rm -rf /var/lib/apt/lists/*
COPY --from=build /build/inst /opt/bluespec/
ENV PATH=/opt/bluespec/bin:$PATH
