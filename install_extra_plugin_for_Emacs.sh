#!/usr/bin/bash

git submodule update --init --recursive

cp -rf ./extra_plugin/ ./.emacs.d/extra


cp -rf .emacs.d ~/.emacs.d

cp .emacs ~/.emacs
