#!/bin/bash

rm -rf out || exit 0;
mkdir out;

GH_REPO="@github.com/angusmoore/arphit.git"

FULL_REPO="https://$GH_TOKEN$GH_REPO"

cd out
git init
git config user.name "travis"
git config user.email "travis"
cp ../arphit/inst/doc/index.html index.html
cp ../arphit/inst/doc/plotting-options.html plotting-options.html
cp ../arphit/inst/doc/todo.html todo.html

git add .
git commit -m "Auto-deployed vignettes to github pages"
git push --force --quiet $FULL_REPO master:gh-pages
