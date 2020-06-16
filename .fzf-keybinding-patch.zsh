# restore keybindings set by .fzf.zsh
bindkey "^R" history-incremental-search-backward
bindkey "^T" transpose-chars
bindkey "^[c" capitalize-word

# bind fzf widgets to undefined keys
bindkey "^[i" fzf-file-widget
bindkey "^[p" fzf-history-widget
bindkey "^[j" fzf-cd-widget
