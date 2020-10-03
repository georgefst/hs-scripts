#!/bin/bash

#TODO move this stuff in to Build.hs

SCRIPTS_DIR=$(pwd)
ARCH_VER=x86_64-linux-8.10.2
ENV_DIR=/home/gthomas/.ghc/$ARCH_VER/environments/

rm $ENV_DIR/scripts

rm -rf git-deps
mkdir git-deps

function cabal-env-git {
    cd git-deps
    git clone $1
    cd $2
    git checkout $4
    cabal-env --local $3 -n scripts
    cd $SCRIPTS_DIR
}

cabal-env-git https://github.com/georgefst/evdev evdev/evdev evdev a3959e51d3e4274ca96c945e81ef7cbb0453511e

#TODO versions - not sure currently possible with cabal-env
    # (but in that case what does '--any' mean?)
cabal-env -n scripts \
    ansi-terminal \
    bytestring \
    directory \
    extra \
    filepath \
    filepath-bytestring \
    ghc \
    hinotify \
    lucid \
    optparse-generic \
    prettyprinter \
    prettyprinter-lucid \
    pretty-simple \
    process \
    rawfilepath \
    safe \
    shake \
    text \
    time \
    unix \

cp $ENV_DIR/scripts $SCRIPTS_DIR/.ghc.environment.$ARCH_VER
