#!/usr/bin/env bash

cd "$(dirname "${BASH_SOURCE}")";

function commands_exist() {
    for c in $@; do
        command -v $c >/dev/null 2>&1 || return $?
    done
}

function install_tpm() {
    if commands_exist tmux git && [ ! -d ~/.tmux/plugins/tpm ]; then
        git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
    fi
}

function platform() {
    local kernel=$(uname -s)
    case $kernel in
        CYGWIN*|MSYS*|MINGW*) echo Windows;;
        Darwin) echo Mac;;
        *) echo $kernel;;
    esac
}

function doIt() {
    rsync -Rpgol \
	  .bash_aliases \
	  .bash_logout \
	  .bash_profile \
	  .bashrc \
	  .profile \
	  .emacs \
	  .emacs.d/sensible-defaults.el \
	  .emacs.d/move-lines.el \
	  .gitconfig \
	  .gitignore_global \
	  .ssh/config \
	  .tmux.conf \
	  .fzf-keybinding-patch.bash \
          .minttyrc \
	  ~
    source ~/.bash_profile
    install_tpm
    # Run platform-specific code
    local setup="$(platform)/setup"
    test -x $setup && $setup
}

if [ "$1" == "--force" -o "$1" == "-f" ]; then
    doIt;
else
    read -p "This may overwrite existing files in your home directory. Are you sure? (y/n) " -n 1;
    echo "";
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        doIt;
    fi;
fi;

unset doIt
unset install_tpm
unset commands_exist
unset platform
