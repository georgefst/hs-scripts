#!/bin/bash

#TODO move this stuff in to Build.hs

SCRIPTS_DIR=$(pwd)
ARCH_VER=x86_64-linux-8.10.7
ENV_DIR=/home/gthomas/.ghc/$ARCH_VER/environments/

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

cabal-env-git https://github.com/cdepillabout/pretty-simple pretty-simple pretty-simple ae8d2f17ba78edba02b83aed9296a461ebec5f79

#TODO versions - not sure currently possible with cabal-env
    # (but in that case what does '--any' mean?)
cabal-env -n scripts \
    ansi-terminal \
    bytestring \
    directory \
    evdev \
    evdev-streamly \
    extra \
    filepath \
    filepath-bytestring \
    ghc \
    hinotify \
    lucid \
    optparse-generic \
    prettyprinter \
    prettyprinter-lucid \
    process \
    rawfilepath \
    safe \
    shake \
    streamly \
    text \
    time \
    unix \

ln -s $ENV_DIR/scripts $SCRIPTS_DIR/.ghc.environment.$ARCH_VER
