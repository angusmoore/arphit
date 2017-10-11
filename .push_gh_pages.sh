#!/bin/bash

rm -rf out || exit 0;
mkdir out;

GH_REPO="@github.com/angusmoore/arphit.git"

FULL_REPO="https://$GH_TOKEN$GH_REPO"

for files in '*.tar.gz'; do
        tar xfz $files
done

cd out
git init
git config user.name "travis"
git config user.email "travis"
cp ../arphit/inst/doc/index.html index.html
cp ../arphit/inst/doc/plotting-options.html plotting-options.html
cp ../arphit/inst/doc/todo.html todo.html
mkdir images
cp ../arphit/inst/doc/simple_example.png images/simple_example.png
cp ../arphit/inst/doc/complex_example.png images/complex_example.png
cp ../arphit/inst/doc/nooptions.png images/nooptions.png
cp ../arphit/inst/doc/lotsofoptions.png images/lotsofoptions.png

git add .
git commit -m "Auto-deploye vignettes to github pages"
git push --force --quiet $FULL_REPO master:gh-pages
