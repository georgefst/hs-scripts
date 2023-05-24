#!/bin/bash

#TODO support fully on Darwin, and delete this branch
# need a way to specify that some libs are Linux-only (evdev, hinotify...)
# `cabal-env` difficult to build due to C deps of a cabal-env dependency
    # https://github.com/haskell-hvr/lzma/issues/21
        # `nix-shell -p xz` doesn't make a difference
        # note that `xz` is the successor to `lzma`
    # so maybe just wait until `cabal env` is implemented
    # this is the reason for almost all of the changes in this file

#TODO move this stuff in to Build.hs

GHC_VER=$(ghc -V | rev | cut -d ' ' -f 1 | rev)
SCRIPTS_DIR=$(pwd)
ARCH_VER=aarch64-$(uname -s | tr '[:upper:]' '[:lower:]')-$GHC_VER
ENV_DIR=/home/gthomas/.ghc/$ARCH_VER/environments

rm $SCRIPTS_DIR/.ghc.environment.$ARCH_VER

cabal install --package-env . --lib \
    aeson \
    aeson-pretty \
    ansi-terminal \
    async \
    bytestring \
    Chart \
    Chart-diagrams \
    colour \
    comonad \
    composition \
    containers \
    dhall \
    diagrams-core \
    diagrams-lib \
    diagrams-svg \
    directory \
    extra \
    filepath \
    filepath-bytestring \
    freer-simple \
    generic-optics \
    ghc \
    hashable \
    hashtables \
    http-client \
    http-client-tls \
    JuicyPixels \
    lens \
    lifx-lan \
    lucid \
    monad-loops \
    mtl \
    mwc-random \
    network \
    network-uri \
    optics \
    optparse-applicative \
    optparse-generic \
    parsec \
    pretty-simple \
    prettyprinter \
    prettyprinter-graphviz \
    prettyprinter-lucid \
    process \
    random \
    raw-strings-qq \
    rawfilepath \
    safe \
    sbv \
    scientific \
    shake \
    split \
    stm \
    streamly \
    streams \
    text \
    time \
    transformers \
    uniplate \
    unix \
    unordered-containers \
    vector \
    vector-algorithms \
    yaml \

# remove boot packages - unnecessary with --lib and causes "Ambiguous module name" errors
for PKG in \
    bytestring \
    directory \
    filepath \
    ghc \
    process \
    text \
    time \
    unix \

do
    PKG_NOVOWELS=$(echo $PKG | sed 's/[aeiou]//g')
    sed -E -i '' "/package-id $PKG_NOVOWELS-([0-9]|.)*$/d" .ghc.environment.$ARCH_VER
done
