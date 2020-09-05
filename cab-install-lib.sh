#!/bin/bash

rm .ghc.env*

cabal install --package-env . --lib \
    directory-1.3.6.1 \
    extra-1.7.7 \
    optparse-generic-1.4.3 \
    pretty-simple-3.3.0.0 \
    safe-0.3.19 \
    shake-0.19.1 \
    time-1.9.3 \
    turtle-1.5.21 \