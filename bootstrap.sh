#!/bin/bash

sed -i 's/module Build (main) where/module Main (main) where/g' Build.hs
cabal -v1 run Build.hs deps
sed -i 's/module Main (main) where/module Build (main) where/g' Build.hs
