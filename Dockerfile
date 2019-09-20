FROM ubuntu:16.04

RUN apt-get update
RUN apt-get install -y curl llvm-5.0 llvm-5.0-dev
RUN curl -sSL https://get.haskellstack.org/ | sh

COPY Setup.hs /Paskell/Setup.hs
COPY stack.yaml /Paskell/stack.yaml
COPY paskell.cabal /Paskell/paskell.cabal
WORKDIR /Paskell
ENV PATH="/usr/lib/llvm-5.0/bin:$PATH"
ENV PATH="$HOME/.local/bin:$PATH"
RUN stack setup && stack build --only-dependencies
# cache for steps above won't be invalidated upon changes to source files

COPY . /Paskell
RUN mkdir /Paskell/build
ENV PATH="/Paskell/build:$PATH"
RUN stack build --copy-bins --local-bin-path /Paskell/build
