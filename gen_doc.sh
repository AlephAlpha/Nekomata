#! /usr/bin/bash

set -e

cd $(dirname $0)

echo "Building Nekomata..."
cabal build

echo "Generating documentation for builtins..."
cabal run Nekomata -- --doc > doc/Builtins.md

echo "Generating documentation for codepage..."
cabal run Nekomata -- --codepage > doc/CodePage.md

echo "Analyzing test cases..."
python analyze.py
