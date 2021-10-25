#! /bin/bash

#sed -i 's/Parse tks = happyRunIdentity/parse tks = happyRunIdentity/g' .stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/tiger/tiger-tmp/Parser.hs 
stack build --executable-profiling --library-profiling

