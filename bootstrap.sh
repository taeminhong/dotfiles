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

function platform() {
	local name=$(expr $(uname) : '^\([a-zA-Z]*\)')
	case $name in
		CYGWIN|MINGW|MSYS) echo "Windows";;
		Darwin) echo "Mac";;
		*) echo $name;;
	esac
}

function doIt() {
	local fzf_patch=".fzf-keybinding-patch.bash"
	rsync --exclude ".git/" \
		--exclude ".DS_Store" \
		--exclude ".osx" \
		--exclude "bootstrap.sh" \
		--exclude "README.md" \
		--exclude "fzf*" \
		--exclude "keyboard" \
		--exclude "Mac" \
		--exclude "Linux" \
		--exclude "Windows" \
		-avh --no-perms . "$(platform)/" ~;
	append_line "[ -f ~/$fzf_patch ] && source ~/$fzf_patch" ~/.fzf.bash "$fzf_patch"
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
