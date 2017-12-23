#!/usr/bin/env bash

cd "$(dirname "${BASH_SOURCE}")";

function platform() {
	name=$(expr $(uname) : '^\([a-zA-Z]*\)')
	case $name in
		CYGWIN|MINGW|MSYS) echo "Windows";;
		Darwin) echo "Mac";;
		*) echo $name;;
	esac
}

function doIt() {
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
