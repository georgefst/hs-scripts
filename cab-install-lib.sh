#!/bin/bash

rm .ghc.env*

cabal install --package-env . --lib \
    ansi-terminal-0.11 \
    directory-1.3.6.1 \
    extra-1.7.7 \
    lucid-2.9.12 \
    optparse-generic-1.4.3 \
    pretty-simple-4.1.0.0 \
    prettyprinter-1.7.0 \
    prettyprinter-lucid-0.1.0.1 \
    safe-0.3.19 \
    shake-0.19.1 \
    time-1.9.3 \
    turtle-1.5.21 \
