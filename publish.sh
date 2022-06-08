#!/bin/bash
echo "Publish $1..."
cd $1
stack build --test --haddock
stack upload .
cabal v2-haddock --builddir="./doc" --haddock-for-hackage --enable-doc
cd doc
cabal upload --publish -d *.tar.gz
cd ..
rm -r doc
echo "Publish done"