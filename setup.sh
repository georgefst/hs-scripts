#!/bin/bash

#TODO move this stuff in to Build.hs

GHC_VER=$(ghc -V | rev | cut -d ' ' -f 1 | rev)
SCRIPTS_DIR=$(pwd)
ARCH_VER=$(uname -m)-$(uname -s | tr '[:upper:]' '[:lower:]')-$GHC_VER
ENV_DIR=/home/gthomas/.ghc/$ARCH_VER/environments

rm $ENV_DIR/scripts
rm $SCRIPTS_DIR/.ghc.environment.$ARCH_VER

#TODO versions - not sure currently possible with cabal-env
    # (but in that case what does '--any' mean?)
cabal-env -n scripts \
    aeson \
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
    evdev \
    evdev-streamly \
    extra \
    filepath \
    filepath-bytestring \
    freer-simple \
    generic-optics \
    ghc \
    hinotify \
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
    rawfilepath \
    safe \
    sbv \
    shake \
    streamly \
    streams \
    text \
    time \
    transformers \
    uniplate \
    unix \
    vector \
    X11 \
    yaml \

ln -s $ENV_DIR/scripts $SCRIPTS_DIR/.ghc.environment.$ARCH_VER

#TODO these don't build with GHC 9.2
    # prettyprinter-graphviz \
        # requires `--allow-newer=graphviz:bytestring`: https://github.com/ivan-m/graphviz/pull/53
