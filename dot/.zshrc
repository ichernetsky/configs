HISTFILE=~/.histfile
HISTSIZE=2000
SAVEHIST=$HISTSIZE
setopt hist_ignore_all_dups
setopt hist_ignore_space

setopt autocd nomatch notify
unsetopt appendhistory beep extendedglob
bindkey -e

zstyle :compinstall filename '/home/ivan/.zshrc'

autoload -Uz compinit
compinit
zstyle ':completion:*:descriptions' format '%U%B%d%b%u'
zstyle ':completion:*:warnings' format '%BSorry, no matches for: %d%b'
# tab completion for PID :D
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*' force-list always

autoload -U promptinit
promptinit

alias ls='ls --color=auto -F'
alias grep='grep --color=auto'

EDITOR="/usr/bin/zile"
export EDITOR

if [ -f ~/.zshrc.local ]; then
    source ~/.zshrc.local
fi
