#!/bin/bash

#TODO move this stuff in to Build.hs

SCRIPTS_DIR=$(pwd)
ARCH_VER=$(uname -m)-$(uname -s | tr '[:upper:]' '[:lower:]')-9.2.1
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

#TODO https://github.com/cdepillabout/pretty-simple/pull/80
cabal-env-git https://github.com/cdepillabout/pretty-simple pretty-simple pretty-simple ae8d2f17ba78edba02b83aed9296a461ebec5f79
#TODO https://github.com/xtendo-org/rawfilepath/pull/5
cabal-env-git https://github.com/georgefst/rawfilepath rawfilepath rawfilepath f609f22e82de36e0e62a9fc6b1368b50614bf3d1

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
    prettyprinter-graphviz \
    prettyprinter-lucid \
    process \
    safe \
    shake \
    streamly \
    text \
    time \
    unix \

ln -s $ENV_DIR/scripts $SCRIPTS_DIR/.ghc.environment.$ARCH_VER
