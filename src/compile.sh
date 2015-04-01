#! /bin/bash

function ngc-compile() { 
ghc -Wall -O2 -o GramCrackers Main.hs -prof -v
}
