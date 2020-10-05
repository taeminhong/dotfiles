#!/bin/sh

# cd to the directory that contains this script.
# reference: https://stackoverflow.com/a/246128
source="$0"
# While $source is a symlink, resolve it
while test -L "$source"
do
    dir="$(cd -P "$( dirname "$source")" && pwd)"
    source="$(readlink "$source")"
    # If $source was a relative symlink (so no "/" as prefix, need to
    # resolve it relative to the symlink base directory
    case "$source" in
        /*) ;;
        *) source="$dir/$source";;
    esac
done
cd "$(dirname "$source")" || exit 1
unset dir
unset source

# The directory in which files will be downloaded.
# This will be deleted on exit.
downloads=$(mktemp -d)

cleanup () {
    rm -rf "$downloads"
}

trap cleanup EXIT

# rsync is more suitable for this task, but it's not available on Git for Windows
cp -a \
   .bash_logout \
   .bash_profile \
   .bashrc \
   .profile \
   .aliases \
   .zprofile \
   .zshrc \
   .vimrc \
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
cp -a .emacs.d/init.el ~/.emacs.d
cp -a .emacs.d/local/taemin.el ~/.emacs.d/local
cp -a .emacs.d/local/untitled-note.el ~/.emacs.d/local

mkdir -p ~/.ssh
cp -a .ssh/config ~/.ssh

# install Vundle, Vim plugin manager
if test ! -e ~/.vim/bundle/Vundle.vim && command -v git >/dev/null 2>&1
then
    git clone https://github.com/VundleVim/Vundle.vim.git \
        ~/.vim/bundle/Vundle.vim
fi

__download_unzip () {
    curl -sSL -o"$3" "$1" && unzip -qd"$2" "$3"
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
