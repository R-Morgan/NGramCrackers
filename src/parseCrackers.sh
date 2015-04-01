#! /bin/bash

function ngc-profile () { 
./GramCrackers -i ../doc/examples/multiParaStory.txt -o processed.txt -b +RTS -s -RTS -p
}

function ngc-pure () { 
./GramCrackers -i ../doc/examples/multiParaStory.txt -o processed.txt -b
}
