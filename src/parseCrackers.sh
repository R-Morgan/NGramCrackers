#! /bin/bash

function ngc-extract-profile () { 
./ngc extract -i ../doc/examples/multiParaStory.txt -o processed.txt -b +RTS -s -RTS -p
}

function ngc-extract-pure () { 
./ngc extract -i ../doc/examples/multiParaStory.txt -o processed.txt -b
}
