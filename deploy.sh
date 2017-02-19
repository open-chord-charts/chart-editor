#!/bin/bash

REV=$(git rev-parse HEAD)

rm -rf dist
rm -rf elm-stuff/build-artifacts # To display all the warnings, do not keep old compilation results.

elm make src/Main.elm --warn --output=dist/main.js

cp index.html dist
sed -i -- 's~/_compile/src/Main.elm~main.js~' dist/index.html
sed -i -- 's~runElmProgram~Elm.Main.fullscreen~' dist/index.html

echo "Press Enter to deploy or Ctrl-C to abort (to fix warnings, to test with 'static dist' server)..."
read

pushd dist
git init
git add .
git commit -m "Build application from commit $REV"
git push --force git@github.com:openchordcharts/chart-editor.git master:gh-pages
popd

# Refresh the gh-pages branch in particular.
git fetch
