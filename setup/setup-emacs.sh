#!/bin/sh

set -eu

DOTFILES_DIR=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
EMACS_DIR="$HOME/.emacs.d"

mkdir -p "$EMACS_DIR"

if [ -e "$EMACS_DIR/init.el" ] || [ -L "$EMACS_DIR/init.el" ]; then
  printf '%s\n' "skip: $EMACS_DIR/init.el already exists"
else
  ln -s "$DOTFILES_DIR/emacs/init.el" "$EMACS_DIR/init.el"
  printf '%s\n' "linked: $EMACS_DIR/init.el"
fi

if [ -e "$EMACS_DIR/config.el" ] || [ -L "$EMACS_DIR/config.el" ]; then
  printf '%s\n' "skip: $EMACS_DIR/config.el already exists"
else
  printf '%s\n' "# Add your secrets here (habitica-uid, gemini-api-key, etc)" > "$EMACS_DIR/config.el"
  chmod 600 "$EMACS_DIR/config.el"
  printf '%s\n' "created: $EMACS_DIR/config.el"
fi
