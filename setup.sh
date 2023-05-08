#!/bin/bash

#TODO move this stuff in to Build.hs

GHC_VER=$(ghc -V | rev | cut -d ' ' -f 1 | rev)
SCRIPTS_DIR=$(pwd)
ARCH_VER=$(uname -m)-$(uname -s | tr '[:upper:]' '[:lower:]')-$GHC_VER
ENV_DIR=/home/gthomas/.ghc/$ARCH_VER/environments

rm $ENV_DIR/scripts
rm $SCRIPTS_DIR/.ghc.environment.$ARCH_VER

#TODO without this, `cabal-env` complains about the config file not existent, i.e. it hasn't been updated for XDG
mkdir $HOME/.cabal
cp $HOME/.config/cabal/config $HOME/.cabal/config
echo "store-dir: $HOME/.local/state/cabal/store" >> $HOME/.cabal/config

#TODO versions - not sure currently possible with cabal-env
    # (but in that case what does '--any' mean?)
cabal-env -n scripts \
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
    network \
    network-uri \
    optics \
    optparse-applicative \
    optparse-generic \
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

#TODO see `mkdir $HOME/.cabal`, above
gio trash $HOME/.cabal

ln -s $ENV_DIR/scripts $SCRIPTS_DIR/.ghc.environment.$ARCH_VER
