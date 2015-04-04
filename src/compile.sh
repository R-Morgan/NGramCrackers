#! /bin/bash

function ngc-compile() { 
ghc -Wall -O2 -o ngc Main.hs -prof -v
}
