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

rm .ghc.environment.$ARCH_VER

#TODO versions - not sure currently possible with cabal-env
    # (but in that case what does '--any' mean?)
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
    http-client \
    http-client-tls \
    JuicyPixels \
    lens \
    lifx-lan \
    lucid \
    monad-loops \
    mtl \
    mwc-random \
    optics \
    optparse-applicative \
    optparse-generic \
    pretty-simple \
    prettyprinter \
    prettyprinter-lucid \
    process \
    random \
    raw-strings-qq \
    rawfilepath \
    safe \
    sbv \
    shake \
    streams \
    text \
    time \
    transformers \
    uniplate \
    unix \
    vector \
    X11 \
    yaml \

# ln -s $ENV_DIR/scripts $SCRIPTS_DIR/.ghc.environment.$ARCH_VER

#TODO these don't build with GHC 9.2
    # prettyprinter-graphviz \
        # requires `--allow-newer=graphviz:bytestring`: https://github.com/ivan-m/graphviz/pull/53

#TODO duplicates (boot packages - unnecessary with --lib)
    # bytestring \
    # directory \
    # filepath \
    # ghc \
    # process \
    # text \
    # time \
    # unix \

#TODO Linux only
    # evdev \
    # evdev-streamly \
    # hinotify \
