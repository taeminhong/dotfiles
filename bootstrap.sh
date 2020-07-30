#!/bin/sh

# cd to the directory that contains this script.
source="$0"
if test -L "$0"
then
    source="$(readlink "$0")"
fi
cd "$(dirname "${source}")" || exit 1
unset source

# directory in which files are saved temporarily.
# this will be deleted on exit.
downloads=$(mktemp -d)

cleanup () {
    rm -rf "$downloads"
}

trap cleanup EXIT

# rsync is more suitable in this task, but it's not available on Git for Windows
cp -a \
   .bash_logout \
   .bash_profile \
   .bashrc \
   .profile \
   .shrc \
   .aliases \
   .zprofile \
   .zshrc \
   .emacs \
   .gitconfig \
   .gitignore_global \
   .fzf-keybinding-patch.bash \
   .fzf-keybinding-patch.zsh \
   .minttyrc \
   .inputrc \
   .ghci \
   tmux.sh \
   ~
mkdir -p ~/.emacs.d/local
cp -a .emacs.d/local/taemin.el ~/.emacs.d/local

mkdir -p ~/.ssh
cp -a .ssh/config ~/.ssh

__download_unzip () {
    curl -sL -o"$3" "$1" && unzip -qd"$2" "$3"
}

download_unzip () {
    __download_unzip "$1" "$2" "$2/$(basename "$1")"
}

# z - directory jump utility
if test ! -e ~/z.sh
then
    download_unzip https://github.com/rupa/z/archive/v1.11.zip "$downloads" && \
        cp "$downloads/z-1.11/z.sh" ~
fi

# sensible-defaults - sensible emacs settings
if test ! -e ~/.emacs.d/local/sensible-defaults.el
then
    download_unzip https://github.com/hrs/sensible-defaults.el/archive/main.zip "$downloads" && \
        cp "$downloads/sensible-defaults.el-main/sensible-defaults.el" ~/.emacs.d/local && \
        echo "(provide 'sensible-defaults)" >>~/.emacs.d/local/sensible-defaults.el
fi

if command -v tmux >/dev/null 2>&1
then
    ./gen-tmux-conf .tmux.conf.in >~/.tmux.conf
    # Install TPM
    if command -v git >/dev/null 2>&1 && test ! -d ~/.tmux/plugins/tpm
    then
        git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
    fi
fi

# FreeBSD's ls doen't support --color option
if ls --color >/dev/null 2>&1
then
    printf "\nalias ls='ls --color=auto'\n" >>~/.aliases
elif ls -G >/dev/null 2>&1
then
    printf "\nalias ls='ls -G'\n" >>~/.aliases
fi

# Run platform-specific code
platform="$(uname -s)"
case $platform in
    CYGWIN*|MSYS*|MINGW*) platform="Windows";;
    Darwin) platform="Mac";;
esac
if test -x "$platform/setup"
then
    "$platform/setup"
fi
