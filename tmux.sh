# When you invoke tmux without any arguments it will try to attach to the default session.
# If there's no such session, it will make a new one with that name.

_tmux () {
    if test $# -eq 0
    then
        \tmux a -t ${TMUX_DEFAULT_SESSION:-home} || \tmux new -s ${TMUX_DEFAULT_SESSION:-home}
    else
        \tmux "$@"
    fi
}
alias tmux=_tmux
