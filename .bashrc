# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# turn off the line editing forcely if it is in the emacs shell
if [[ "$INSIDE_EMACS" =~ ",comint" ]]; then
    set +o emacs
    set +o vi
fi

if [ -n "$INSIDE_EMACS" ]; then
    GIT_EDITOR=false
fi

# don't put duplicate lines or lines starting with space in the history.
HISTCONTROL=ignoreboth
HISTSIZE=1000
HISTFILESIZE=2000

# append to the history file, don't overwrite it
shopt -s histappend
# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize
# Autocorrect typo in path names when `cd`ing
shopt -s cdspell

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

__ps1_symbol () {
    if [ $? = 0 ]; then
        printf "$1"
    else
        printf "$2"
    fi
}

PS1="${debian_chroot:+<$debian_chroot> }"
case "$TERM" in
    eterm-color|xterm-color|*-256color)
        PS1+="${SSH_CLIENT:+\[\e[32m\]\u@\h }"
        PS1+="\[\e[34m\]\W\[\e[m\] \`__ps1_symbol \$ \[\e[31m\]!\[\e[m\]\` "
        ;;
    *)
        PS1+="${SSH_CLIENT:\u@\h }\W \`__ps1_symbol \$ !\`"
        ;;
esac

# https://github.com/akermu/emacs-libvterm?tab=readme-ov-file#shell-side-configuration
vterm_printf() {
    if [ -n "$TMUX" ] \
        && { [ "${TERM%%-*}" = "tmux" ] \
            || [ "${TERM%%-*}" = "screen" ]; }; then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
    # Linux bash-completion
    if [ -f /usr/share/bash-completion/bash_completion ]; then
        . /usr/share/bash-completion/bash_completion
    elif [ -f /etc/bash_completion ]; then
        . /etc/bash_completion
    fi
    # MacPort bash-completion
    if [ -f /opt/local/etc/profile.d/bash_completion.sh ]; then
        . /opt/local/etc/profile.d/bash_completion.sh
    fi
fi

if [[ ":$SHELLOPTS:" =~ :emacs: ]]; then
    # Don't let the terminal hijack C-s, C-q, and C-w keys
    stty -ixon werase undef
    bind '"\C-w": kill-region'
    bind '"\ew": copy-region-as-kill'
    # Command line fuzzy finder: https://github.com/junegunn/fzf
    if [ -f ~/.fzf.bash ]; then
        source ~/.fzf.bash
        source ~/.fzf-keybinding-patch.bash
    fi
fi

test -f ~/.local/bin/aws_completer && complete -C "$_" aws

# NVM
if [ -d ~/.nvm ]; then
    export NVM_DIR="$HOME/.nvm"
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
    # nvm bash_completion
    [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"
fi

if [ -e ~/miniconda3/bin/conda ]; then
    conda() {
        unset -f conda

        # >>> conda initialize >>>
        # !! Contents within this block are managed by 'conda init' !!
        __conda_setup="$("$HOME/miniconda3/bin/conda" 'shell.bash' 'hook' 2> /dev/null)"
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

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'
# enable color support of ls
if command -v dircolors >/dev/null 2>&1; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
fi

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
if command -v notify-send >/dev/null 2>&1
then
    alert () {
        notify-send --urgency=low -i "$(test $? = 0 && echo terminal || echo error)" \
                    "$(history | tail -n1 | sed -e 's/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//')"
    }
fi

. ~/.aliases
. ~/tmux.sh

if [ -f ~/z.sh ]; then
    . ~/z.sh
fi

if [ -f ~/.nix-profile/etc/profile.d/nix.sh ]; then
    . ~/.nix-profile/etc/profile.d/nix.sh
fi
