#!/usr/bin/bash

git submodule update --init --recursive

mkdir -p .emacs.d/extra

cp -rf ./extra_plugin/* ./.emacs.d/extra

mv ~/.emacs.d ~/.emacs.d.bak

cp -rf .emacs.d ~/.emacs.d

mv ~/.emacs.d/extra/extra/* ~/.emacs.d/extra

cp .emacs ~/.emacs
