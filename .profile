# This file is sourced by many kinds of shells, like bash, dash and zsh
# So be aware of portability.
# Git coding guidelines will help you write portable shell scripts
# https://github.com/git/git/blob/master/Documentation/CodingGuidelines

: ${INITIAL_PATH=$PATH}
: ${INITIAL_C_INCLUDE_PATH=$C_INCLUDE_PATH}
: ${INITIAL_CPLUS_INCLUDE_PATH=$CPLUS_INCLUDE_PATH}
: ${INITIAL_LIBRARY_PATH=$LIBRARY_PATH}

PATH="$INITIAL_PATH"
C_INCLUDE_PATH="$INITIAL_C_INCLUDE_PATH"
CPLUS_INCLUDE_PATH="$INITIAL_CPLUS_INCLUDE_PATH"
LIBRARY_PATH="$INITIAL_LIBRARY_PATH"

# Prepend the given path to the variable.
# usage: addpath VAR PATH
addpath () {
    eval "test -d \"$2\" && $1=\"$2\${$1:+:\$$1}\""
}

# MacPort
addpath PATH /opt/local/bin
addpath PATH /opt/local/sbin
# Homebrew
if test -x /opt/homebrew/bin/brew
then
    HOMEBREW_PREFIX="/opt/homebrew/"
    HOMEBREW_CELLAR="/opt/homebrew/Cellar"
    HOMEBREW_REPOSITORY="/opt/homebrew"
    addpath PATH /opt/homebrew/sbin
    addpath PATH /opt/homebrew/bin
elif test -x /usr/local/bin/brew
then
    HOMEBREW_PREFIX="/usr/local"
    HOMEBREW_CELLAR="/usr/local/Cellar"
    HOMEBREW_REPOSITORY="/usr/local/Homebrew"
    addpath PATH /usr/local/sbin
fi
# Cargo, a package manager for Rust
addpath PATH "$HOME/.cargo/bin"
# Yarn, a package manager for JS
addpath PATH "$HOME/.yarn/bin"
# Haskell
addpath PATH "$HOME/.ghcup/bin"
addpath PATH "$HOME/.cabal/bin"
# systemd user bin directory
addpath PATH "$HOME/.local/bin"

addpath C_INCLUDE_PATH     /opt/local/include
addpath CPLUS_INCLUDE_PATH /opt/local/include
addpath LIBRARY_PATH       /opt/local/lib

unset -f addpath

export LANG=en_US.UTF-8
export LC_ALL="$LANG"
export INITIAL_PATH
export INITIAL_C_INCLUDE_PATH
export INITIAL_CPLUS_INCLUDE_PATH
export INITIAL_LIBRARY_PATH

test -n "$C_INCLUDE_PATH" && export C_INCLUDE_PATH || unset C_INCLUDE_PATH
test -n "$CPLUS_INCLUDE_PATH" && export CPLUS_INCLUDE_PATH || unset CPLUS_INCLUDE_PATH
test -n "$LIBRARY_PATH" && export LIBRARY_PATH || unset LIBRARY_PATH
test -n "$HOMEBREW_PREFIX" && export HOMEBREW_PREFIX
test -n "$HOMEBREW_CELLAR" && export HOMEBREW_CELLAR
test -n "$HOMEBREW_REPOSITORY" && export HOMEBREW_REPOSITORY
