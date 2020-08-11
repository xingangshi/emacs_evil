#!/usr/bin/bash

# for debian
apt-get install emacs

# for centos
yum install emacs

git submodule update --init --recursive

cp -rf ./extra_plugin/ ./.emacs.d/extra

cp -rf .emacs.d ~/.emacs.d

cp .emacs ~/.emacs
