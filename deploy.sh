#!/bin/bash

rm -rf dist
elm-make src/Main.elm --warn --output=dist/index.html
cd dist
git init
git add .
git commit -m "Deploy to Github Pages"
git push git@github.com:openchordcharts/chart-editor.git master:gh-pages
