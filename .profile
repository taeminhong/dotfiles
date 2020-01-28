# This file is sourced by many kinds of shells, like bash, dash and zsh
# So be aware of portability.
# Git coding guidelines will help you write portable shell scripts
# https://github.com/git/git/blob/master/Documentation/CodingGuidelines

export DEFAULT_PATH=${DEFAULT_PATH:-$PATH}
PATH=$DEFAULT_PATH

addpath () {
    test -d "$1" && PATH="$1:$PATH"
 }

# MacPort
addpath /opt/local/bin
addpath /opt/local/sbin
addpath "/Applications/Racket v7.5/bin"
# allow running commands installed by `pip install --user` on the Mac
addpath "$HOME/Library/Python/3.6/bin"
# Cargo, a package manager for Rust
addpath "$HOME/.cargo/bin"
# Yarn, a package manager for JS
addpath "$HOME/.yarn/bin"

unset addpath
