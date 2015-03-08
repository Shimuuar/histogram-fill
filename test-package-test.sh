#!/bin/sh
set -e # Bail on first error
set -v # Be verbose

cd "$1"
cabal install   --enable-tests --only-dependencies
cabal configure --enable-tests -v2  # -v2 provides useful information for debugging
cabal build
cabal test
