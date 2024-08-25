#!/bin/sh

ln -sf ~/dotfiles/.zshrc ~/
touch ~/dotfiles/zsh/config.zsh

mkdir -p ~/.config
ln -sf ~/dotfiles/nvim ~/.config
ln -sf ~/dotfiles/init.el ~/.emacs.d/init.el
