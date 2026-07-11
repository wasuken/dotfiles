#!/bin/sh

# Zsh
ln -sf ~/dotfiles/.zshrc ~/
touch ~/dotfiles/zsh/config.zsh

# Neovim (if still using)
mkdir -p ~/.config
NVIM_DIR="$HOME/.config/nvim"
if [ -d "$NVIM_DIR/.git" ]; then
  git -C "$NVIM_DIR" pull
else
  git clone git@github.com:wasuken/nvim.git "$NVIM_DIR"
fi

# Emacs
mkdir -p ~/.emacs.d
ln -sf ~/dotfiles/emacs/init.el ~/.emacs.d/init.el

# Create secret files
touch ~/.emacs.d/config.el
echo "# Add your secrets here (habitica-uid, gemini-api-key, etc)" >>~/.emacs.d/config.el
