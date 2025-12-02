# turn off the line editing forcely if it is in the emacs shell
if [[ "$INSIDE_EMACS" =~ ".*,comint$" ]]; then
    unsetopt zle
fi

# Remove '/' and '-' from the WORDCHARS
WORDCHARS='*?_.[]~=&;!#$%^(){}<>'

# change sigil to a red exclamation mark if the last command has failed.
PROMPT='%F{blue}%1~%f %(?.%#.%F{red}!%f) '
# show username and hostname if SSH-ing
if [ -n "$SSH_CLIENT" ]; then
    PROMPT="%F{green}%n@%m%f $PROMPT"
fi

# deactivate mark after copy-region-as-kill, like Emacs
# https://unix.stackexchange.com/a/19956
copy-region-as-kill-deactivate-mark() {
    zle copy-region-as-kill
    zle set-mark-command -n -1
}

if [[ $options[zle] = on ]]; then
    # Don't let the terminal hijack C-s, C-q, and C-w keys
    stty -ixon werase undef
    zle -N copy-region-as-kill-deactivate-mark
    bindkey "^w" kill-region
    bindkey "^[w" copy-region-as-kill-deactivate-mark
    bindkey "^u" backward-kill-line
    if [ -f ~/.fzf.zsh ]; then
        . ~/.fzf.zsh
        . ~/.fzf-keybinding-patch.zsh
    fi
fi

if [ -e ~/miniconda3/bin/conda ]; then
    conda() {
        unset -f conda

        # >>> conda initialize >>>
        # !! Contents within this block are managed by 'conda init' !!
        __conda_setup="$("$HOME/miniconda3/bin/conda" 'shell.zsh' 'hook' 2> /dev/null)"
        if [ $? -eq 0 ]; then
            eval "$__conda_setup"
        else
            if [ -f "$HOME/miniconda3/etc/profile.d/conda.sh" ]; then
                . "$HOME/miniconda3/etc/profile.d/conda.sh"
            elif [ -d "$HOME/miniconda3/bin" ]; then
                export PATH="$HOME/miniconda3/bin:$PATH"
            fi
        fi
        unset __conda_setup
        # <<< conda initialize <<<

        conda "$@"
    }
fi

autoload -Uz compinit && compinit

. ~/.aliases
. ~/tmux.sh

if [ -f ~/z.sh ]; then
    . ~/z.sh
fi

if [ -f ~/.nix-profile/etc/profile.d/nix.sh ]; then
    . ~/.nix-profile/etc/profile.d/nix.sh
fi
