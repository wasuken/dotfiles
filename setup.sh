#!/bin/sh

set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)

for setup_script in \
  setup-emacs.sh \
  setup-lazyvim.sh \
  setup-zsh.sh \
  setup-sway.sh
do
  "$SCRIPT_DIR/setup/$setup_script"
done
