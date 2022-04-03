#!/bin/bash

#TODO move this stuff in to Build.hs

SCRIPTS_DIR=$(pwd)
ARCH_VER=$(uname -m)-$(uname -s | tr '[:upper:]' '[:lower:]')-9.2.1
ENV_DIR=/home/gthomas/.ghc/$ARCH_VER/environments

rm $ENV_DIR/scripts
rm $SCRIPTS_DIR/.ghc.environment.$ARCH_VER
rm -rf git-deps ; mkdir git-deps

function cabal-env-git {
    cd git-deps
    git clone $1
    cd $2
    git checkout $4
    cabal-env --local $3 -n scripts
    cd $SCRIPTS_DIR
}

#TODO versions - not sure currently possible with cabal-env
    # (but in that case what does '--any' mean?)
cabal-env -n scripts \
    ansi-terminal \
    bytestring \
    colour \
    composition \
    containers \
    directory \
    evdev \
    evdev-streamly \
    extra \
    filepath \
    filepath-bytestring \
    generic-optics \
    ghc \
    hinotify \
    lens \
    lifx-lan \
    lucid \
    monad-loops \
    mtl \
    mwc-random \
    optics \
    optparse-generic \
    prettyprinter \
    prettyprinter-lucid \
    process \
    random \
    safe \
    shake \
    streamly \
    text \
    time \
    transformers \
    uniplate \
    unix \
    vector \

ln -s $ENV_DIR/scripts $SCRIPTS_DIR/.ghc.environment.$ARCH_VER

#TODO these don't build with GHC 9.2
    # prettyprinter-graphviz \
        # requires `--allow-newer=graphviz:bytestring`: https://github.com/ivan-m/graphviz/pull/53
