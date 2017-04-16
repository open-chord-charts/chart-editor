#!/usr/bin/env sh

DIST_DIR="dist"

if [ ! -d "$DIST_DIR" ]; then
    echo "$DIST_DIR does not exist"
    exit 1
fi

if [ "$CI" = "true" ]; then
    git config --global user.email "deploy@circleci"
    git config --global user.name "CircleCI deployment"
fi

cp circle-gh-pages.yml index.html style.css "$DIST_DIR"
sed -i -- 's~/_compile/src/Main.elm~main.js~' "$DIST_DIR"/index.html
sed -i -- 's~runElmProgram~Elm.Main.fullscreen~' "$DIST_DIR"/index.html

gh-pages --dist "$DIST_DIR"
