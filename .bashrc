# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

. ~/.shrc

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# append to the Bash history file, rather than overwriting it
shopt -s histappend;

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

_build_prompt () {
    local exitcode=$?
    local sigil='\$'
    if [ "$exitcode" != 0 ]; then
        sigil='!'
    fi
    local chroot="${debian_chroot:+($debian_chroot)}"
    # set a fancy prompt (non-color, unless we know we "want" color)
    # if colors is not empty, ${colors[0]} should refer the default color.
    # for details of ANSI escape code, visit https://en.wikipedia.org/wiki/ANSI_escape_code
    local colors=()
    case "$TERM" in
        xterm-color|*-256color)
            colors=('\[\033[00m\]' '\[\033[01;32m\]' '\[\033[01;34m\]');;
    esac
    PS1="${TERM_TITLE}${chroot}${colors[1]}\u@\h ${colors[2]}\W${colors[0]} ${sigil} "
}
PROMPT_COMMAND=_build_prompt

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

# Command line fuzzy finder: https://github.com/junegunn/fzf
if [ -o emacs -a -f ~/.fzf.bash ]; then
    source ~/.fzf.bash
    source ~/.fzf-keybinding-patch.bash
fi
# fd supports --exclude option from version 5.0.0
if fd -d 0 --exclude .git >/dev/null 2>&1; then
    # find all files include hidden ones
    export FZF_DEFAULT_COMMAND='fd --type f --hidden --exclude .git -d 7'
else
    export FZF_DEFAULT_COMMAND='fd --type f --hidden -d 7 | grep -v "^.git"'
fi
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"

# NVM
if [ -d ~/.nvm ]; then
    export NVM_DIR="$HOME/.nvm"
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
    # nvm bash_completion
    [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"
fi

# MacPort bash-completion
if [ -f /opt/local/etc/profile.d/bash_completion.sh ]; then
    . /opt/local/etc/profile.d/bash_completion.sh
fi

# enable color support of ls
if command -v dircolors >/dev/null 2>&1; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
fi

# Autocorrect typo in path names when `cd`ing
shopt -s cdspell

. ~/z.sh
