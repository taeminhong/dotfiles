# This file is sourced by many kinds of shells, like bash, dash and zsh
# So be aware of portability.
# Git coding guidelines will help you write portable shell scripts
# https://github.com/git/git/blob/master/Documentation/CodingGuidelines

# Reset the variable to the initial state
# usage: resetvar VAR
resetvar () {
    eval ": \"\${INITIAL_$1=\$$1}\"
          $1=\"\$INITIAL_$1\"
          export INITIAL_$1"
}

# Prepend the given path to the variable.
# usage: addpath VAR PATH
addpath () {
    test -d "$2" && eval "$1=\"$2\${$1:+:\$$1}\""
}

# Export variable if it is defined and non-zero
# usage: exportnz VAR
exportnz () {
    eval "test -n \"\$$1\" && export $1"
}

resetvar PATH
resetvar C_INCLUDE_PATH
resetvar CPLUS_INCLUDE_PATH
resetvar LIBRARY_PATH

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

addpath PATH "$HOME/.dotnet/tools"

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

export LANG=en_US.UTF-8
export LC_ALL="$LANG"

exportnz C_INCLUDE_PATH || unset C_INCLUDE_PATH
exportnz CPLUS_INCLUDE_PATH || unset CPLUS_INCLUDE_PATH
exportnz LIBRARY_PATH || unset LIBRARY_PATH
exportnz HOMEBREW_PREFIX
exportnz HOMEBREW_CELLAR
exportnz HOMEBREW_REPOSITORY

unset -f resetvar addpath exportnz
