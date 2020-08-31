# Prevent forward-search keybinding from being overriden by XON/XOFF flow control
stty -ixon

# Remove '/' and '-' from the WORDCHARS
WORDCHARS='*?_.[]~=&;!#$%^(){}<>'

# change sigil to red exclamation mark if the last command has failed.
short_prompt='%F{blue}%1~ %(?.%f%b%#.%F{red}!%f%b) '
# show username and hostname only for remote sessions
if [ -n "$SSH_CLIENT" ]
then
    PROMPT='%B%F{green}%n@%m '"$short_prompt"
else
    PROMPT="$short_prompt"
fi
unset short_prompt

if [ -z "$INSIDE_EMACS" ] && [ -f ~/.fzf.zsh ]; then
    . ~/.fzf.zsh
    . ~/.fzf-keybinding-patch.zsh
fi

. ~/.aliases
. ~/tmux.sh
. ~/z.sh
