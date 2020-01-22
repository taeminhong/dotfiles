#!/bin/sh

commands_exist () {
    for c in "$@"
    do
        command -v "$c" >/dev/null 2>&1 || return $?
    done
}

install_tpm () {
    if commands_exist tmux git && test ! -d ~/.tmux/plugins/tpm
    then
        git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
    fi
}

platform () {
    kernel=$(uname -s)
    case $kernel in
        CYGWIN*|MSYS*|MINGW*) echo Windows;;
        Darwin) echo Mac;;
        *) echo "$kernel";;
    esac
    unset kernel
}

execute () {
    test -x "$1" && "$1"
}

# cd to the directory that contains this script.
source="$0"
if test -L "$0"
then
    source="$(readlink "$0")"
fi
cd "$(dirname "${source}")" || exit 1
unset source

# rsync is more suitable in this task, but it's not available on Git for Windows
cp -a \
   .bash_aliases \
   .bash_logout \
   .bash_profile \
   .bashrc \
   .profile \
   .zprofile \
   .zshrc \
   .emacs \
   .gitconfig \
   .gitignore_global \
   .tmux.conf \
   .fzf-keybinding-patch.bash \
   .minttyrc \
   .inputrc \
   z.sh \
   ~
mkdir -p ~/.emacs.d && \
    cp -a .emacs.d/sensible-defaults.el .emacs.d/move-lines.el ~/.emacs.d
mkdir -p ~/.ssh && \
    cp -a .ssh/config ~/.ssh

install_tpm
# Run platform-specific code
execute "$(platform)/setup"
