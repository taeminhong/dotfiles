# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

addpath() {
    if [ -n "$1" -a -d "$1" ]; then
        case ":$PATH:" in
            *":$1:"*) ;;
            *) PATH="$1:$PATH";;
        esac
    fi
}

# MacPort
addpath /opt/local/bin
addpath /opt/local/sbin
# allow running commands installed by `pip install --user` on the Mac
addpath "$HOME/Library/Python/3.4/bin"
# Cargo, a package manager for Rust
addpath "$HOME/.cargo/bin"
# Yarn, a package manager for JS
addpath "$HOME/.yarn/bin"

unset addpath
