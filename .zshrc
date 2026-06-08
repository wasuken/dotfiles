zmodload zsh/zprof
# Enable Powerlevel10k instant prompt.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# 履歴ファイルの設定
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt share_history
setopt hist_ignore_dups
setopt hist_no_store
setopt hist_reduce_blanks

sync_history() {
    fc -W
}

source "$HOME/.zinit/bin/zinit.zsh"
typeset -g ZINIT[OPTIMIZE_OUT_DISK_ACCESSES]=1
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit
zinit light-mode for \
    zdharma-continuum/z-a-patch-dl \
    zdharma-continuum/z-a-as-monitor \
    zdharma-continuum/z-a-bin-gem-node
### End of Zinit's installer chunk
zinit light zsh-users/zsh-autosuggestions
zinit wait lucid for zdharma-continuum/fast-syntax-highlighting
zinit wait lucid for zdharma-continuum/history-search-multi-word
zinit ice from"gh-r" as"program"
# zinit load junegunn/fzf-bin
zinit wait lucid atload"zicompinit; zicdreplay" blockf for zsh-users/zsh-completions
zplugin ice depth=1; zplugin light romkatv/powerlevel10k

[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
[[ ! -f ~/dotfiles/zsh/config.zsh ]] || source ~/dotfiles/zsh/config.zsh
add-zsh-hook precmd sync_history

export PATH="$PATH:/home/wasu/.local/bin"
export PATH=$PATH:$HOME/.maestro/bin
