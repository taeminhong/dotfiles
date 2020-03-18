# When you invoke tmux without any arguments it will try to attach to the most
# recently used session.
# If there's no session, it will make a new one with the default session name.

_tmux () {
    if test $# -eq 0
    then
        \tmux a || \tmux new -s ${TMUX_DEFAULT_SESSION:-home}
    else
        \tmux "$@"
    fi
}
alias tmux=_tmux
