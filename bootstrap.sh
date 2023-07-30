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
   .jmacsrc \
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
cp -a .emacs.d/local/taemin*.el ~/.emacs.d/local
cp -a .emacs.d/local/untitled-note.el ~/.emacs.d/local
cp -a .emacs.d/local/blank.el ~/.emacs.d/local

mkdir -p ~/.ssh
cp -a .ssh/config ~/.ssh

mkdir -p ~/.local/bin

# install vim-plug, a minimalist Vim plugin manager
if test ! -e ~/.vim/autoload/plug.vim
then
    curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
         https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
fi
# remove Vundle if it exists
rm -rf ~/.vim/bundle/Vundle.vim
rmdir ~/.vim/bundle >/dev/null 2>&1

# z - directory jump utility
if test ! -e ~/z.sh || test ! -f ~/.local/share/man/man1/z.1
then
    curl -sSL -o "$downloads/z-1.11.tar.gz" "https://github.com/rupa/z/archive/refs/tags/v1.11.tar.gz" &&
        tar xzf "$downloads/z-1.11.tar.gz" -C "$downloads" &&
        cp "$downloads/z-1.11/z.sh" ~ &&
        mkdir -p ~/.local/share/man/man1 &&
        cp "$downloads/z-1.11/z.1" ~/.local/share/man/man1
fi

if command -v tmux >/dev/null 2>&1
then
    cp -a .tmux.conf ~
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
