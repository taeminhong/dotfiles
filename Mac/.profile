# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

addpath() {
    case ":$PATH:" in
        *":$1:"*) :;;
        *) PATH="$1:$PATH";;
    esac
}

# MacPort
addpath /opt/local/bin
addpath /opt/local/sbin

# allow running commands installed by `pip install --user`
addpath ~/Library/Python/3.4/bin

# Cargo is a package manager for Rust
addpath ~/.cargo/bin
