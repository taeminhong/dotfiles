# Remove '/' and '-' from the WORDCHARS
WORDCHARS='*?_.[]~=&;!#$%^(){}<>'

# change sigil to a red exclamation mark if the last command has failed.
PROMPT='%F{blue}%1~%f %(?.%#.%F{red}!%f) '
# show username and hostname if SSH-ing
if [ -n "$SSH_CLIENT" ]; then
    PROMPT="%F{green}%n@%m%f $PROMPT"
fi

if [[ $options[zle] = on && -z "$INSIDE_EMACS" ]]; then
    # Prevent forward-search keybinding from being overriden by START/STOP flow control
    stty -ixon
    if [ -f ~/.fzf.zsh ]; then
        . ~/.fzf.zsh
        . ~/.fzf-keybinding-patch.zsh
    fi
fi

. ~/.aliases
. ~/tmux.sh
. ~/z.sh
