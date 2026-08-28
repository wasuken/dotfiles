#!/bin/sh

set -eu

NVIM_DIR="$HOME/.config/nvim"

if [ -e "$NVIM_DIR" ] || [ -L "$NVIM_DIR" ]; then
  printf '%s\n' "skip: $NVIM_DIR already exists"
  exit 0
fi

mkdir -p "$HOME/.config"
git clone git@github.com:wasuken/nvim.git "$NVIM_DIR"
printf '%s\n' "cloned: $NVIM_DIR"
