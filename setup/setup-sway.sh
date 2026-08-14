#!/bin/sh

set -eu

DOTFILES_DIR=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
SWAY_DIR="$HOME/.config/sway"

mkdir -p "$SWAY_DIR"

if [ -e "$SWAY_DIR/config" ] || [ -L "$SWAY_DIR/config" ]; then
  printf '%s\n' "skip: $SWAY_DIR/config already exists"
else
  ln -s "$DOTFILES_DIR/sway/config" "$SWAY_DIR/config"
  printf '%s\n' "linked: $SWAY_DIR/config"
fi

if [ -e "$SWAY_DIR/config.d" ] || [ -L "$SWAY_DIR/config.d" ]; then
  printf '%s\n' "skip: $SWAY_DIR/config.d already exists"
else
  ln -s "$DOTFILES_DIR/sway/config.d" "$SWAY_DIR/config.d"
  printf '%s\n' "linked: $SWAY_DIR/config.d"
fi
