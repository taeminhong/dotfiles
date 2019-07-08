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
    local name=$(expr $(uname) : '^\([a-zA-Z]*\)')
    case $name in
        CYGWIN|MINGW|MSYS) echo "Windows";;
        Darwin) echo "Mac";;
        *) echo $name;;
    esac
}

function make_target() {
    local target=$(platform)
    if [ -d "$target" ]; then
        echo "$target"
    else
        echo default
    fi
}

function doIt() {
    local workspace=$(mktemp -d)
    trap "rm -rf $workspace" EXIT
    make workspace=$workspace $(make_target) && cp -a $workspace/. ~ && install_tpm && source ~/.bash_profile;
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
unset doIt;
