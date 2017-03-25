#!/bin/bash

npm run build

if [ $? != 0 ]; then
    echo "Build failed, abandon deploy."
    exit 1
fi

cp index.html dist
sed -i -- 's~/_compile/src/Main.elm~main.js~' dist/index.html
sed -i -- 's~runElmProgram~Elm.Main.fullscreen~' dist/index.html

gh-pages --dist dist

# Refresh the gh-pages branch in particular.
git fetch
