#!/bin/sh

STOWDOTDIR=~/dotfiles2

git clone https://github.com/gmarik/Vundle.vim.git $STOWDOTDIR/vim/.vim/bundle/vundle

# there's probably an easier way to list the packages, but this works
for i in $(find . -maxdepth 1 -type d -not -name ".*" -printf "%P\n")
do
  stow  $i
done

