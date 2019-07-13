cd "$(dirname "${BASH_SOURCE}")"

function commands_exist {
    for c in $@; do
        command -v $c >/dev/null 2>&1 || return $?
    done
}

function install_tpm {
    if commands_exist tmux git && [ ! -d ~/.tmux/plugins/tpm ]; then
        git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
    fi
}

function platform {
    local kernel=$(uname -s)
    case $kernel in
        CYGWIN*|MSYS*|MINGW*) echo Windows;;
        Darwin) echo Mac;;
        *) echo $kernel;;
    esac
}

function execute {
    test -x "$1" && "$1"
}

# rsync is more suitable in this task, but it's not available on Git for Windows
cp -a \
   .bash_aliases \
   .bash_logout \
   .bash_profile \
   .bashrc \
   .profile \
   .emacs \
   .gitconfig \
   .gitignore_global \
   .tmux.conf \
   .fzf-keybinding-patch.bash \
   .minttyrc \
   ~
mkdir -p ~/.emacs.d && \
    cp -a .emacs.d/{sensible-defaults.el,move-lines.el} ~/.emacs.d
mkdir -p ~/.ssh && \
    cp -a .ssh/config ~/.ssh
source ~/.bash_profile
install_tpm
# Run platform-specific code
execute "$(platform)/setup"

unset install_tpm
unset commands_exist
unset platform
unset execute
