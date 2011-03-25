HISTFILE=~/.histfile
HISTSIZE=2000
SAVEHIST=$HISTSIZE
setopt hist_ignore_all_dups
setopt hist_ignore_space

bindkey "^[[2~" yank
bindkey "^[[3~" delete-char
bindkey "^[[5~" up-line-or-history
bindkey "^[[6~" down-line-or-history
bindkey "^[[7~" beginning-of-line #xterm
bindkey "^[[H"  beginning-of-line #ttyv
bindkey "^[[1~" beginning-of-line #konsole

bindkey "^[[8~" end-of-line
bindkey "^[[4~" end-of-line
bindkey "^[[F"  end-of-line
bindkey "^[e" expand-cmd-path ## C-e for expanding path of typed command
bindkey "^[[A" up-line-or-search ## up arrow for back-history-search
bindkey "^[[B" down-line-or-search ## down arrow for fwd-history-search
bindkey " " magic-space ## do history expansion on space
bindkey "^R" history-incremental-search-backward

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

zmodload -a zsh/stat stat
zmodload -a zsh/zpty zpty
zmodload -a zsh/zprof zprof
zmodload -ap zsh/mapfile mapfile
zstyle ':completion:*:processes' command 'ps -xuf'
zstyle ':completion:*:processes' sort false
zstyle ':completion:*:processes-names' command 'ps xho command'
zstyle ':completion:*:cd:*' ignore-parents parent pwd
zstyle -e ':completion:*:approximate:*' max-errors 'reply=( $(( ($#PREFIX+$#SUFFIX)/3 )) numeric )'
zstyle ':completion:*' menu select=long-list select=0
zstyle ':completion:*' old-menu true
zstyle ':completion:*' original true
zstyle ':completion:*' substitute 1
zstyle ':completion:*' use-compctl true
zstyle ':completion:*' verbose true
zstyle ':completion:*' word true
compctl -C -c + -K compctl_rehash + -c

autoload -U promptinit
promptinit

autoload -U zmv

alias ls='ls --color=auto -F'
alias grep='grep --color=auto'

EDITOR="/usr/bin/zile"
export EDITOR

if [ -f ~/.zshrc.local ]; then
    source ~/.zshrc.local
fi
