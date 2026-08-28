#!/bin/sh

set -eu

DOTFILES_DIR=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
ZSHRC="$HOME/.zshrc"

if [ -e "$ZSHRC" ] || [ -L "$ZSHRC" ]; then
  printf '%s\n' "skip: $ZSHRC already exists"
else
  ln -s "$DOTFILES_DIR/.zshrc" "$ZSHRC"
  printf '%s\n' "linked: $ZSHRC"
fi

if [ -e "$DOTFILES_DIR/zsh/config.zsh" ]; then
  printf '%s\n' "skip: $DOTFILES_DIR/zsh/config.zsh already exists"
else
  : > "$DOTFILES_DIR/zsh/config.zsh"
  printf '%s\n' "created: $DOTFILES_DIR/zsh/config.zsh"
fi
