# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
HISTCONTROL=ignoreboth
HISTSIZE=1000
HISTFILESIZE=2000

# append to the history file, don't overwrite it
shopt -s histappend
# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize
# append to the Bash history file, rather than overwriting it
shopt -s histappend
# Autocorrect typo in path names when `cd`ing
shopt -s cdspell

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

_build_prompt () {
    local exitcode=$?
    local chroot="${debian_chroot:+($debian_chroot)}"
    # set a fancy prompt (non-color, unless we know we "want" color)
    # for details of ANSI escape code, visit https://en.wikipedia.org/wiki/ANSI_escape_code
    local reset red green blue
    case "$TERM" in
        xterm-color|*-256color)
            reset='\[\033[00m\]'
            green='\[\033[00;32m\]'
            blue='\[\033[00;34m\]'
            red='\[\033[00;31m\]'
            ;;
    esac
    PS1="${TERM_TITLE}${chroot}"
    if [ -n "$SSH_CLIENT" ]; then
        PS1+="${green}\u@\h "
    fi
    if [ "$exitcode" = 0 ]; then
        PS1+="${blue}\W ${reset}\\$ "
    else
        PS1+="${blue}\W ${red}!${reset} "
    fi
}
PROMPT_COMMAND=_build_prompt

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

# check if line editing is enabled
# https://github.com/rycee/home-manager/issues/401#issuecomment-434058700
if [[ ":$SHELLOPTS:" =~ :emacs: && -z "$INSIDE_EMACS" ]]; then
    # Prevent forward-search keybinding from being overriden by START/STOP output control.
    stty -ixon
    # Command line fuzzy finder: https://github.com/junegunn/fzf
    if [ -f ~/.fzf.bash ]; then
        source ~/.fzf.bash
        source ~/.fzf-keybinding-patch.bash
        # fd supports --exclude option from version 5.0.0
        if fd -d 0 --exclude .git >/dev/null 2>&1; then
            # find all files include hidden ones
            export FZF_DEFAULT_COMMAND='fd --type f --hidden --exclude .git -d 7'
        else
            export FZF_DEFAULT_COMMAND='fd --type f --hidden -d 7 | grep -v "^.git"'
        fi
        export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
    fi
fi

# NVM
if [ -d ~/.nvm ]; then
    export NVM_DIR="$HOME/.nvm"
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
    # nvm bash_completion
    [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"
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
. ~/z.sh
