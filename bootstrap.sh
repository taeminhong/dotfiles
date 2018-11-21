#!/usr/bin/env bash

cd "$(dirname "${BASH_SOURCE}")";

function append_line() {
    set -e

    local line="$1"
    local file="$2"
    local pattern="$3"
    local lineno=""

    if [ -f "$file" ]; then
        lineno=$(grep -nF "$pattern" "$file" | sed 's/:.*//' | tr '\n' ' ')
        if [ -z "$lineno" ]; then
            echo "$line" >> "$file"
        fi
    fi

    set +e
}

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

function doIt() {
    local workspace=$(mktemp -d)
    local fzf_patch=".fzf-keybinding-patch.bash"
    rsync --exclude ".git/" \
          --exclude ".DS_Store" \
          --exclude ".osx" \
          --exclude "bootstrap.sh" \
          --exclude "append.sh" \
          --exclude "README.md" \
          --exclude "Mac" \
          --exclude "Linux" \
          --exclude "Windows" \
          -ah --no-perms . $workspace

    local dir=$(platform)
    [ -d "$dir" ] && find "$dir" -type f -not -name ".DS_Store" -not -name ".osx" \
            | sed -e p -e "s#^$dir#$workspace#" \
            | tr "\n" "\0" \
            | xargs -0 -n2 ./append.sh

    rsync -cavh --no-perms $workspace/ ~
    rm -rf $workspace

    append_line "[ -f ~/$fzf_patch ] && source ~/$fzf_patch" ~/.fzf.bash "$fzf_patch"
    install_tpm
    source ~/.bash_profile;
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
