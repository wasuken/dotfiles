#!/bin/sh

# Zsh
ln -sf ~/dotfiles/.zshrc ~/
touch ~/dotfiles/zsh/config.zsh

# Neovim (if still using)
mkdir -p ~/.config
ln -sf ~/dotfiles/nvim ~/.config/

# Emacs
ln -sf ~/dotfiles/emacs ~/.emacs.d

# Create secret files
touch ~/.emacs.d/config.el
echo "# Add your secrets here (habitica-uid, gemini-api-key, etc)" >> ~/.emacs.d/config.el
