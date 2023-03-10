# Ctrl-R -> Alt-P
bindkey -M emacs    "^R"  history-incremental-search-backward
bindkey -M emacs    "^[p" fzf-history-widget
bindkey -M vicmd    "^R"  redo
bindkey -M vicmd    "^[p" fzf-history-widget
bindkey -M viins    "^R"  redisplay
bindkey -M viins    "^[p" fzf-history-widget

# Alt-C -> Alt-J
bindkey -M emacs    "^[c" capitalize-word
bindkey -M emacs    "^[j" fzf-cd-widget
bindkey -M vicmd -r "^[c"
bindkey -M vicmd    "^[j" fzf-cd-widget
bindkey -M viins -r "^[c"
bindkey -M viins    "^[j" fzf-cd-widget

# Ctrl-T -> Alt-I
bindkey -M emacs    "^T"  transpose-chars
bindkey -M emacs    "^[i" fzf-file-widget
bindkey -M vicmd -r "^T"
bindkey -M vicmd    "^[i" fzf-file-widget
bindkey -M viins    "^T"  self-insert
bindkey -M viins    "^[i" fzf-file-widget
