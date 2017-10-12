#!/bin/bash

rm -rf out || exit 0;
mkdir out;

GH_REPO="@github.com/angusmoore/arphit.git"

FULL_REPO="https://$GH_TOKEN$GH_REPO"

for files in '*.tar.gz'; do
        tar xfz $files
done

R -e "path <- './'; system(paste(shQuote(file.path(R.home('bin'), 'R')), 'CMD', 'Rd2pdf', shQuote(path)))"

R CMD INSTALL "$(find . -type f -iname 'arphit*.tar.gz' -print -quit)"
Rscript ./build-readme-images.R

cd out
git init
git config user.name "travis"
git config user.email "travis"
cp ../arphit/inst/doc/index.html index.html
cp ../arphit/inst/doc/plotting-options.html plotting-options.html
cp ../arphit/inst/doc/todo.html todo.html

cp ../arphit.pdf arphit.pdf

mkdir images
cp ../simple_example.png images/simple_example.png
cp ../complex_example.png images/complex_example.png
cp ../nooptions.png images/nooptions.png
cp ../lotsofoptions.png images/lotsofoptions.png

git add .
git commit -m "Auto-deploy vignettes to github pages"
git push --force --quiet $FULL_REPO master:gh-pages
